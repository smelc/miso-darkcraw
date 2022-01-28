{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- |
-- This module deals with game actions (nicknamed moves). It's a spinoff of
-- 'Update', to reduce the length of the latter.
module Move
  ( DnDAction (..),
    Move (..),
    NextSched,
    runOne,
    Sched (..),
  )
where

import qualified AI
import qualified Board
import BoardInstances (boardStart)
import Control.Arrow ((>>>))
import Control.Monad.Except (MonadError, runExcept)
import Control.Monad.Identity (runIdentity)
import qualified Data.Bifunctor as Bifunctor
import Data.Either.Extra
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Debug.Trace (traceShow)
import qualified Game
import Miso.String (MisoString)
import Model
import Nat
import qualified Spots
import qualified Turn

-- | Actions that can be scheduled by the main loop.
data Sched
  = -- | Play some game event. It can be an event scheduled by the AI
    -- or an event from the player.
    Play Game.Event
  | -- | Turn was updated previously by 'IncrTurn',
    -- time to draw cards from the stack. Then the handler of this event
    -- will take care of giving the player control back. This event
    -- is translated to a list of events, iteratively consuming the list.
    DrawCards Game.DrawSource
  | -- | All actions have been resolved, time to update the turn widget
    -- and to schedule 'DrawCard'. This does NOT translate
    -- to a 'PlayEvent'.
    IncrTurn
  | -- | End Turn button pressed in turn widget. For player, schedule
    -- attacks then 'IncrTurn'; for AI, compute its actions,
    -- schedule them, and then schedule attack and 'IncrTurn'.
    EndTurnPressed
  | -- A number of events to apply in sequence.
    Sequence (NonEmpty Sched)
  deriving (Show, Eq)

data DnDAction a
  = -- | Dragging card in hand ends. When a successful drop is done,
    -- this event is fired right after 'Drop'. We rely on that. If Drop
    -- was fired last, we would miss it. Be careful on untested browsers.
    DragEnd
  | -- | Dragging card in hand
    DragStart Board.HandIndex
  | -- | In 'GameView', this can play the 'GamePlayEvent' 'Place'
    Drop
  | DragEnter a
  | DragLeave a
  deriving (Show, Eq)

-- | Actions that are raised by 'GameView'
data Move
  = -- | A drag an drop event
    DnD (DnDAction Game.Target)
  | -- | Starting hovering card in hand
    InHandMouseEnter Board.HandIndex
  | -- | Ending hovering card in hand
    InHandMouseLeave Board.HandIndex
  | -- | Starting hovering a target
    InPlaceMouseEnter Game.Target
  | -- | Ending hovering a target
    InPlaceMouseLeave Game.Target
  | -- | Execute a command (dev mode only)
    ExecuteCmd
  | -- | A schedulable event
    Sched Sched
  | -- | Update the command to execute soon (dev mode only)
    UpdateCmd MisoString
  deriving (Show, Eq)

mkScheds :: [Sched] -> Maybe Sched
mkScheds l =
  case NE.nonEmpty l of
    Nothing -> Nothing
    Just l -> Just (Sequence l)

-- | Event to fire after the given delay (in seconds). Delay should not be '0'.
type NextSched = Maybe (Nat, Sched)

-- | Given (maybe) an action to schedule and other actions, build the sequence
cons :: NextSched -> [Sched] -> NextSched
cons nga scheds =
  case (nga, NE.nonEmpty scheds) of
    (Nothing, Nothing) -> Nothing
    (Nothing, Just neActions) -> Just (1, Sequence neActions)
    (Just pair, Nothing) -> Just pair
    (Just (n, ga), Just neActions) -> Just (n, Sequence (ga NE.<| neActions))

runOne ::
  MonadError Text.Text m =>
  GameModel ->
  Sched ->
  m (GameModel, NextSched)
runOne m (Sequence (fst NE.:| rest)) = do
  (m', nga) <- runOne m fst
  return (m', cons nga rest)
runOne m@GameModel {board, shared} (Play gameEvent) = do
  (shared', board', anims', generated) <- Game.playE shared board gameEvent
  let anim =
        Game.eventToAnim shared board gameEvent
          & runExcept
          & eitherToMaybe
          & fromMaybe Game.NoAnimation -- Rather no animation that erroring out
      m' = m {board = board', shared = shared', anims = anims', anim}
      nextEvent =
        case (gameEvent, generated) of
          (Game.Attack pSpot cSpot continue changeTurn, Nothing) ->
            -- enqueue resolving next attack if applicable
            case (continue, Game.nextAttackSpot board pSpot (Just cSpot)) of
              (False, _) -> terminator
              (True, Nothing) -> terminator
              (True, Just cSpot') -> Just $ Move.Play $ Game.Attack pSpot cSpot' True changeTurn
            where
              terminator = if changeTurn then Just Move.IncrTurn else Nothing
          (Game.Attack {}, Just e) ->
            error $ "Cannot mix Game.Attack and events when enqueueing but got event: " ++ show e
          (_, _) -> Move.Play <$> generated
  -- There MUST be a delay here, otherwise it means we would need
  -- to execute this event now. We don't want that. 'playAll' checks that.
  pure $ (m', (1,) <$> nextEvent)
runOne m@GameModel {board, shared, turn} (DrawCards draw) = do
  pure $
    ( m {anims = boardui', board = board', shared = shared'},
      Nothing
    )
  where
    (shared', board', boardui') = Game.drawCard shared board pSpot draw
    pSpot = Turn.toPlayerSpot turn
-- "End Turn" button pressed by the player or the AI
runOne m@GameModel {board, difficulty, playingPlayer, shared, turn} Move.EndTurnPressed = do
  m@GameModel {board} <-
    ( if isInitialTurn
        then do
          -- End Turn pressed at the end of the player's first turn, make the AI
          -- place its card in a state where the player did not put its
          -- card yet, then place them all at once; and then continue
          -- Do not reveal player placement to AI
          let emptyPlayerInPlaceBoard = Board.setInPlace board pSpot Map.empty
              placements =
                AI.play
                  difficulty
                  shared
                  emptyPlayerInPlaceBoard
                  (turn & Turn.next & Turn.toPlayerSpot)
          (shared, board, anims) <- Game.playAllE shared board $ map Game.PEvent placements
          pure $ m {anims, board, shared}
        else pure m
      )
      <&> disableUI
  let sched = mkSched board
  if isInitialTurn
    then -- We want a one second delay, to see clearly that the opponent
    -- puts its cards, and then proceed with resolving attacks
      pure $ (m, Just (1, sched))
    else -- We don't want any delay so that the game feels responsive
    -- when the player presses "End Turn", hence the recursive call.
      runOne m sched
  where
    pSpot = Turn.toPlayerSpot turn
    isInitialTurn = turn == Turn.initial
    disableUI gm = if pSpot == playingPlayer then gm {uiAvail = False} else gm
    mkSched b =
      -- schedule resolving first attack
      case Game.nextAttackSpot b pSpot Nothing of
        Nothing -> Move.IncrTurn -- no attack, change turn right away
        Just cSpot -> Move.Play $ Game.Attack pSpot cSpot True True
runOne m Move.IncrTurn =
  m & incrTurn & Bifunctor.first enableUI & pure
  where
    enableUI gm@GameModel {playingPlayer, turn}
      | Turn.toPlayerSpot turn == playingPlayer =
        gm {uiAvail = True} -- Restore interactions if turn of player
      | otherwise = gm

-- TODO @smelc write a test that this function always returns GameEndTurnPressed
-- when it's the AI turn.
incrTurn ::
  GameModel ->
  (GameModel, NextSched)
incrTurn m@GameModel {board, difficulty, playingPlayer, shared, turn} = runIdentity $ do
  board <- pure $ boardStart board pSpot
  (shared, board, anims) <- pure $ Game.transferCards shared board pSpot
  let drawSrcs = Game.cardsToDraw board pSpot True
      -- If it's the player turn, we wanna draw the first card right away,
      -- so that the game feels responsive.
      drawNow = if isAI then drawSrcs else take 1 drawSrcs
  (shared, board, anims) <-
    pure $
      Game.drawCards shared board pSpot drawNow
        & (\(s, b, a) -> (s, b, a <> anims)) -- Don't forget earlier 'anims'
  let preTurnEvents =
        Game.keepEffectfull
          shared
          board
          [ Game.ApplyFearNTerror otherSpot,
            Game.ApplyBrainless pSpot,
            Game.FillTheFrontline pSpot,
            Game.ApplyKing pSpot
          ]
  let actions :: [Sched] =
        -- AI case: after drawing cards and playing its events
        --          press "End Turn". We want a one second delay, it makes
        --          it easier to understand what's going on
        --
        --          Also, we need to apply 'preTurnEvents' before computing the
        --          AI's events.
        -- player case: we drew the first card already (see drawNow),
        --              enqueue next event (if any)
        case (isAI, Game.playAll shared board preTurnEvents) of
          (True, Left errMsg) -> traceShow ("AI cannot play:" ++ Text.unpack errMsg) []
          (True, Right (Game.Result {board = board'})) ->
            let plays :: [Game.Place] = AI.play difficulty shared board' pSpot
             in case NE.nonEmpty plays of
                  Nothing -> []
                  Just plays -> [plays & NE.map (Game.PEvent >>> Move.Play) & Move.Sequence]
          (False, _) -> Prelude.drop 1 drawSrcs & map Move.DrawCards
  actions <-
    pure $
      if isAI
        then actions ++ [Move.EndTurnPressed] -- THe AI MUST press end turn, no matter what
        -- So better do it here than in the different cases above.
        else actions
  m <- pure $ m {anims, board, shared, turn = turn'}
  pure $ (m, (1,) <$> Move.mkScheds actions)
  where
    turn' = Turn.next turn
    pSpot = Turn.toPlayerSpot turn'
    otherSpot = Spots.otherPlayerSpot pSpot
    isAI = pSpot /= playingPlayer
