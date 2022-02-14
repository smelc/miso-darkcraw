{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- This module deals with game actions (nicknamed moves). It's a spinoff of
-- 'Update', to reduce the length of the latter.
module Move
  ( Actor (..),
    DnDAction (..),
    Kernel,
    MakeHandlers (..),
    Move (..),
    NextSched,
    runAll,
    runAllMaybe,
    runOne,
    runOneModel,
    Sched (..),
    startTurn,
  )
where

import qualified AI
import Board (Board)
import qualified Board
import BoardInstances (boardStart)
import Card
import Contains (Contains, with)
import qualified Contains
import Control.Monad.Except (MonadError, runExcept)
import Control.Monad.Identity (runIdentity)
import qualified Data.Bifunctor as Bifunctor
import Data.Either.Extra
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Text as Text
import qualified Game
import Miso.String (MisoString)
import Model
import Nat
import SharedModel (SharedModel)
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

-- | Drag and drop actions
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

toMaybe :: [Sched] -> Maybe Sched
toMaybe l =
  case NE.nonEmpty l of
    Nothing -> Nothing
    Just (x NE.:| []) -> Just x
    Just l -> Just (Sequence l)

eventsToSched :: [Game.Event] -> Maybe Sched
eventsToSched events =
  case NE.nonEmpty events of
    Nothing -> Nothing
    Just (event NE.:| []) -> Just $ Play $ event
    Just events -> Just $ Sequence $ NE.map Play events

-- | Transform a @Maybe Sched@ into a 'NextSched'
nextify :: Maybe Sched -> NextSched
nextify m = (1,) <$> m

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

-- | The subset of 'Model.Game' required by @run*@ functions /!\ If a field
-- is added, extend the @Contains Model.Game Kernel@ instance.
data Kernel = Kernel
  { anim :: Game.Animation,
    anims :: Board 'UI,
    board :: Board 'Core,
    difficulty :: AI.Difficulty,
    playingPlayer :: Spots.Player,
    shared :: SharedModel,
    turn :: Turn.Turn,
    uiAvail :: Bool
  }

instance Contains Kernel (SharedModel, Board 'Core, Board 'UI) where
  to Kernel {shared, board, anims} = (shared, board, anims)
  with m (s, b, a) = m {shared = s, board = b, anims = a}

instance Contains Kernel (SharedModel, Board 'Core, Board 'UI, Game.Animation) where
  to Kernel {shared, board, anims, anim} = (shared, board, anims, anim)
  with m (s, b, a, an) = m {shared = s, board = b, anims = a, anim = an}

instance Contains Model.Game Kernel where
  to Model.Game {..} = Kernel {..}
  with m Kernel {anim, anims, board, difficulty, playingPlayer, shared, turn, uiAvail} =
    m {anim, anims, board, difficulty, playingPlayer, shared, turn, uiAvail}

-- | Making 'runOne' and friends customizable
data Handlers a = Handlers
  { -- | Disable UI while AI is playing
    disableUI :: a -> a,
    -- | Renable UI after control is given back to player
    enableUI :: a -> a
  }

-- | Simple class for building 'Handlers' values
class MakeHandlers a where
  make :: Handlers a

runOne :: MonadError Text.Text m => a ~ Kernel => Sched -> Handlers a -> a -> m (a, NextSched)
runOne (Sequence (fst NE.:| rest)) h m = do
  (m', nga) <- runOne fst h m
  return (m', cons nga rest)
runOne (Play gameEvent) _ m@Kernel {board, shared} = do
  (shared', board', anims', generated) <- Game.playE shared board gameEvent
  let anim =
        Game.eventToAnim shared board gameEvent
          & runExcept
          & eitherToMaybe
          & fromMaybe Game.NoAnimation -- Rather no animation that erroring out
      m' = m `with` (shared', board', anims', anim)
      nextEvent =
        case (gameEvent, generated) of
          (Game.Attack pSpot cSpot continue changeTurn, Nothing) ->
            -- enqueue resolving next attack if applicable; if not terminate
            -- the turn by sending 'IncrTurn'
            case (continue, Game.nextAttackSpot board pSpot (Just cSpot)) of
              (False, _) -> terminator
              (True, Nothing) -> terminator
              (True, Just cSpot') -> Just $ Play $ Game.Attack pSpot cSpot' True changeTurn
            where
              terminator = if changeTurn then Just IncrTurn else Nothing
          (Game.Attack {}, Just e) ->
            error $ "Cannot mix Game.Attack and events when enqueueing but got event: " ++ show e
          (_, _) -> Play <$> generated
  -- There MUST be a delay here, otherwise it means we would need
  -- to execute this event now. We don't want that. 'playAll' checks that.
  pure $ (m', (1,) <$> nextEvent)
runOne (DrawCards draw) _ m@Kernel {board, shared, turn} = do
  pure (m `with` Game.drawCard shared board (Turn.toPlayerSpot turn) draw, Nothing)
-- "End Turn" button pressed by the player or the AI
runOne EndTurnPressed h@Handlers {disableUI} m@Kernel {board, difficulty, shared, turn} = do
  m@Kernel {board} <-
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
          triplet@(_s, _b, _a) <- Game.playAllE shared board $ map Game.PEvent placements
          pure $ m `with` triplet
        else pure m
      )
      <&> disableUI
  let sched :: Sched = mkAttack board
  if isInitialTurn
    then -- We want a one second delay, to see clearly that the opponent
    -- puts its cards, and then proceed with resolving attacks
      pure $ (m, nextify $ Just sched)
    else -- We don't want any delay so that the game feels responsive
    -- when the player presses "End Turn", hence the recursive call.
      runOne sched h m
  where
    pSpot = Turn.toPlayerSpot turn
    isInitialTurn = turn == Turn.initial
    mkAttack b =
      -- schedule resolving first attack
      case Game.nextAttackSpot b pSpot Nothing of
        Nothing -> IncrTurn -- no attack, change turn right away
        Just cSpot -> Play $ Game.Attack pSpot cSpot True True -- There's an attack to resolve
        -- enqeue it. When we handle the attack ('Play gameEvent' above), we will
        -- schedule the terminator 'IncrTurn'
runOne IncrTurn Handlers {enableUI} m@Kernel {playingPlayer, turn} =
  m' & startTurn actor pSpot <&> Bifunctor.first enableUI
  where
    turn' = Turn.next turn
    m' = m {turn = turn'} :: Kernel
    pSpot = Turn.toPlayerSpot turn'
    isAI :: Bool = pSpot /= playingPlayer
    actor :: Actor = if isAI then AI else Player

-- | Like 'runOne', but on 'Model.Game'
runOneModel :: MonadError Text.Text m => a ~ Model.Game => Sched -> a -> m (a, NextSched)
runOneModel s m = do
  (k', s) <- Move.runOne s make k
  pure (m `Contains.with` k', s)
  where
    k = Contains.to m

-- | @runAll m s@ executes @s@ and then continues executing the generated
-- 'NextSched', if any. Returns when executing a 'Sched' doesn't yield a new one.
runAll :: MonadError Text.Text m => a ~ Kernel => Sched -> Handlers a -> a -> m a
runAll s h m = do
  (m', next) <- runOne s h m
  case next of
    Nothing -> pure m'
    Just (_, s') -> runAll s' h m'

-- | @runAllMaybe m s@ executes @s@ (if it is @Just _@) and then continues executing the generated
-- 'NextSched', if any. Returns when 's' is 'Nothing' or when executing the
-- underlying 'Sched' doesn't yield a new one.
runAllMaybe :: MonadError Text.Text m => a ~ Kernel => NextSched -> a -> m a
runAllMaybe s m = case s of Nothing -> pure m; Just (_, s) -> runAll s make m

instance MakeHandlers Kernel where
  make = Handlers {disableUI, enableUI}
    where
      disableUI k@Kernel {playingPlayer, turn}
        | Turn.toPlayerSpot turn == playingPlayer = k {uiAvail = False} :: Kernel
        | otherwise = k
      enableUI k@Kernel {playingPlayer, turn}
        | Turn.toPlayerSpot turn == playingPlayer = k {uiAvail = True} :: Kernel -- Restore interactions if turn of player
        | otherwise = k

-- | Events to execute when the turn of the given 'Spots.Player' starts
preTurnEvents :: Spots.Player -> [Game.Event]
preTurnEvents pSpot =
  [ Game.ApplyFearNTerror $ Spots.other pSpot,
    Game.ApplyBrainless pSpot,
    Game.FillTheFrontline pSpot,
    Game.ApplyKing pSpot
  ]

data Actor = AI | Player

-- | The layer above 'startAITurn' and 'startPlayerTurn'. It intentionally
-- requires the 'Actor' AND 'Spots.Player' so that the AI can be used
-- even when there are 1 or 2 players.
startTurn ::
  MonadError Text.Text m => a ~ Kernel => Actor -> Spots.Player -> a -> m (a, NextSched)
startTurn a pSpot m@Kernel {board = b, turn} =
  ( case a of
      AI -> startAITurn m' pSpot
      Player -> pure $ startPlayerTurn m' pSpot
  )
    <&> Bifunctor.first postStart
  where
    m'
      | turn == Turn.initial = m -- Don't increment stupidity counter for example
      | otherwise = m {board = boardStart b pSpot} :: Kernel

-- | This function is related to 'startAITurn'. If you change
-- this function, consider changing 'startAITurn' too.
startPlayerTurn :: a ~ Kernel => a -> Spots.Player -> (a, NextSched)
startPlayerTurn m@Kernel {board, shared} pSpot = runIdentity $ do
  (shared, board, anims) <- pure $ Game.transferCards shared board pSpot
  -- We draw the first card right away,
  -- so that the game feels responsive when the player turn starts
  let drawNow = Game.cardsToDraw board pSpot True & take 1
      preTurnEs :: [Game.Event] = Game.keepEffectfull shared board $ preTurnEvents pSpot
  (shared, board, anims) <-
    pure $
      Game.drawCards shared board pSpot drawNow
        & (\(s, b, a) -> (s, b, a <> anims)) -- Don't forget earlier 'anims'
  let scheds = map DrawCards drawNow ++ (preTurnEs & eventsToSched & maybeToList)
  return (m `with` (shared, board, anims), scheds & toMaybe & nextify)

-- | This function is related to 'startPlayerTurn'. If you change
-- this function, consider changing 'startPlayerTurn' too.
startAITurn ::
  MonadError Text.Text m => a ~ Kernel => a -> Spots.Player -> m (a, NextSched)
startAITurn m@Kernel {board, difficulty, shared} pSpot = do
  (shared, board, anims) <- pure $ Game.transferCards shared board pSpot
  let drawSrcs :: [Game.DrawSource] = Game.cardsToDraw board pSpot True
      preTurnEs :: [Game.Event] = Game.keepEffectfull shared board $ preTurnEvents pSpot
  (shared, board, anims) <-
    pure $
      Game.drawCards shared board pSpot drawSrcs
        & (\(s, b, a) -> (s, b, a <> anims)) -- Don't forget earlier 'anims'
  let m' = m `with` (shared, board, anims) -- Create m' now, next events will be scheduled, not executed asap
  (afterPreTurnEsShared, afterPreTurnEsBoard, _) <-
    Game.playAllE shared board preTurnEs -- We want 'AI.play' to be executed on the state after pre turn events
  let placeScheds :: [Sched] =
        -- Use 'after pre turn events' data for playing the AI
        AI.play difficulty afterPreTurnEsShared afterPreTurnEsBoard pSpot
          & map (Play . Game.PEvent)
      nextSched = (preTurnEs & eventsToSched & nextify) `cons` (placeScheds ++ [EndTurnPressed])
  pure (m', nextSched)

-- | Function to call after 'startPlayerTurn' and 'startAITurn'
postStart :: a ~ Kernel => a -> a
postStart = id