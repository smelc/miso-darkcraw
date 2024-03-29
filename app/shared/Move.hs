{-# LANGUAGE AllowAmbiguousTypes #-}
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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- This module deals with game actions (nicknamed moves). It's a spinoff of
-- 'Update', to reduce the length of the latter.
module Move
  ( Actor (..),
    Kernel (board),
    mkSimKernel,
    Move (..),
    nextify,
    NextSched,
    -- Don't export @run*@ functions directly, use the @sim@ or @prod@ prefix!
    simRunAllMaybe,
    prodRunOneModel,
    Sched (..),
    startTurn,
    mkPreEndTurnEvents,
  )
where

import {-# SOURCE #-} qualified AI (play)
import qualified Board
import BoardInstances (boardStart)
import Card
import qualified Constants
import Contains (with)
import qualified Contains
import Control.Monad.Except (MonadError (throwError), runExcept)
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
import qualified Shared
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

-- | Actions that are raised by 'GameView'
data Move
  = -- | Starting hovering a box
    MouseEnter Box
  | -- | Ending hovering a box
    MouseLeave Box
  | -- | Execute a command (dev mode only)
    ExecuteCmd
  | -- | A schedulable event
    Sched Sched
  | Selection Model.Box
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
-- is added, extend the two @Contains Model.Game (Kernel _)@instances. When running
-- in production, @a@ is 'Spots.Player'. When in tests, @()@ should be used,
-- as there is no playing player.
data Kernel a = Kernel
  { anim :: Game.Animation,
    anims :: Board.T 'UI,
    board :: Board.T 'Core,
    difficulty :: Constants.Difficulty,
    playingPlayer :: a,
    shared :: Shared.Model,
    turn :: Turn.T,
    uiAvail :: Bool
  }

-- | Creates a minimal kernel, suited for simulation
mkSimKernel :: Constants.Difficulty -> Shared.Model -> Turn.T -> Board.T 'Core -> Kernel ()
mkSimKernel difficulty shared turn board = Kernel {..}
  where
    anim = Game.NoAnimation
    anims = mempty
    playingPlayer = ()
    uiAvail = False

instance Contains.Contains (Kernel a) (Shared.Model, Board.T 'Core, Board.T 'UI) where
  to Kernel {shared, board, anims} = (shared, board, anims)

instance Contains.With (Kernel a) (Shared.Model, Board.T 'Core, Board.T 'UI) where
  with m (s, b, a) = m {shared = s, board = b, anims = a}

instance Contains.Contains (Kernel a) (Shared.Model, Board.T 'Core, Board.T 'UI, Game.Animation) where
  to Kernel {shared, board, anims, anim} = (shared, board, anims, anim)

instance Contains.With (Kernel a) (Shared.Model, Board.T 'Core, Board.T 'UI, Game.Animation) where
  with m (s, b, a, an) = m {shared = s, board = b, anims = a, anim = an}

instance Contains.Contains Model.Game (Kernel Spots.Player) where
  to Model.Game {..} = Kernel {playingPlayer = Model.pSpot player, ..}

instance Contains.With Model.Game (Kernel Spots.Player) where
  with
    m@(Model.Game {player})
    Kernel {anim, anims, board, difficulty, playingPlayer, shared, turn, uiAvail} =
      m {anim, anims, board, difficulty, player = player', shared, turn, uiAvail}
      where
        player' = player {pSpot = playingPlayer}

instance Contains.Contains Model.Game (Kernel ()) where
  to Model.Game {..} = Kernel {playingPlayer = (), ..}

instance Contains.With Model.Game (Kernel ()) where
  with m Kernel {anim, anims, board, difficulty, shared, turn, uiAvail} =
    m {anim, anims, board, difficulty, shared, turn, uiAvail}

-- | Making 'runOne' and friends customizable
data Handlers a = Handlers
  { -- | Disable UI while AI is playing
    disableUI :: a -> a,
    -- | Renable UI after control is given back to player
    enableUI :: a -> a,
    -- | Increment the turn counter. Must rely on 'incrTurnBase'
    incrTurn :: a -> Either Text.Text (a, NextSched)
  }

-- | Data whose only purpose is to be lifted as a type, for making
-- production and simulation instances selection explicit.
data HandlersKind = Prod | Sim

-- | Simple class for building 'Handlers' values
class MakeHandlers (b :: HandlersKind) a where
  make :: Handlers a

runOne :: MonadError Text.Text m => a ~ Kernel b => Sched -> Handlers a -> a -> m (a, NextSched)
runOne (Sequence (fst NE.:| rest)) h m = do
  (m', nga) <- runOne fst h m
  return (m', cons nga rest)
runOne (Play gameEvent) _ m@Kernel {board, shared, turn} = do
  (shared', board', anims', generated) <- Game.playE shared $ Game.Playable board gameEvent turn
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
          let emptyPlayerInPlaceBoard = Board.setInPlace pSpot Map.empty board
              turn' = Turn.next turn
              placements =
                -- FIXME @smelc only allow to place creatures and items (no spell)
                AI.play
                  difficulty
                  shared
                  emptyPlayerInPlaceBoard
                  (Turn.toPlayerSpot turn')
                  turn'
          triplet@(_s, _b, _a) <- Game.playAllE shared $ Game.Playable board (map Game.PEvent placements) turn
          pure $ m `with` triplet
        else pure m
      )
      <&> disableUI
  let es = mkEvents PreEndTurn shared turn pSpot board
  boardAfterEs <- Game.playAllE shared (Game.Playable board es turn) <&> (\(_, b', _) -> b')
  let sched :: NextSched = cons (nextify $ eventsToSched es) [mkAttack boardAfterEs]
  case (isInitialTurn, sched) of
    -- In case 'isInitialTurn' holds, we want a one second delay, to see clearly
    -- that the opponent puts its cards, and then proceed with resolving attacks. This delay
    -- is introduced by 'cons' above.
    (True, _) -> pure $ (m, sched)
    (False, Nothing) -> pure $ (m, sched)
    (False, Just (_delay, sched)) ->
      -- We don't want any delay for the first event, so that the game feels responsive
      -- when the player presses "End Turn", hence the recursive call.
      runOne sched h m
  where
    pSpot = Turn.toPlayerSpot turn
    isInitialTurn = turn == Turn.initial
    mkAttack :: Board.T 'Core -> Sched
    mkAttack b =
      -- schedule resolving first attack
      case Game.nextAttackSpot b pSpot Nothing of
        Nothing -> IncrTurn -- no attack, change turn right away
        Just cSpot -> Play $ Game.Attack pSpot cSpot True True -- There's an attack to resolve
        -- enqeue it. When we handle the attack ('Play gameEvent' above), we will
        -- schedule the terminator 'IncrTurn'
runOne IncrTurn Handlers {incrTurn} m =
  case incrTurn m of
    Left msg -> throwError msg
    Right pair -> pure pair

-- | Run one event, suited for production, because it keeps track of the playing
-- player under the hood; thanks to the @Kernel Spots.Player@ instance.
prodRunOneModel :: MonadError Text.Text m => a ~ Model.Game => Sched -> a -> m (a, NextSched)
prodRunOneModel s m = do
  (k', s) <- Move.runOne s (make @ 'Prod) k
  pure (m `Contains.with` k', s)
  where
    k :: Kernel Spots.Player = Contains.to m

-- | @runAll m s@ executes @s@ and then continues executing the generated
-- 'NextSched', if any. Returns when executing a 'Sched' doesn't yield a new one.
runAll :: MonadError Text.Text m => a ~ Kernel b => Sched -> Handlers a -> a -> m a
runAll s h m = do
  (m', next) <- runOne s h m
  case next of
    Nothing -> pure m'
    Just (_, s') -> runAll s' h m'

-- | @simRunAllMaybe m s@ executes @s@ (if it is @Just _@) and then continues executing the generated
-- 'NextSched', if any. Returns immediately when 's' is 'Nothing', if 's' is @Just _@ execute it,
-- as well as the generated @Sched@ values, until it runs out of @Sched@ values.
--
-- Only suited for simulation/testing because it doesn't keep track of the playing player
-- and doesn't trigger the AI automatically. It is up to the caller to do it.
simRunAllMaybe :: MonadError Text.Text m => a ~ Kernel () => NextSched -> a -> m a
simRunAllMaybe s m = case s of Nothing -> pure m; Just (_, s) -> runAll s (make @ 'Sim) m

-- | Function that should be called in every 'incrTurn' definition.
incrTurnBase :: Kernel a -> Kernel a
incrTurnBase k@Kernel {turn} = k {turn = Turn.next turn}

-- | The instance for production, that plays the AI in 'incrTurn'
instance MakeHandlers 'Prod (Kernel Spots.Player) where
  make = Handlers {disableUI, enableUI, incrTurn}
    where
      disableUI k@Kernel {playingPlayer, turn}
        | Turn.toPlayerSpot turn == playingPlayer = k {Move.uiAvail = False}
        | otherwise = k
      enableUI k@Kernel {playingPlayer, turn}
        | Turn.toPlayerSpot turn == playingPlayer = k {Move.uiAvail = True} -- Restore interactions if turn of player
        | otherwise = k
      incrTurn k@Kernel {playingPlayer} =
        -- Production case: startTurn is called, triggering the AI
        k' & startTurn actor pSpot & runExcept <&> Bifunctor.first enableUI
        where
          k'@Kernel {turn = turn'} = incrTurnBase k
          pSpot = Turn.toPlayerSpot turn'
          isAI :: Bool = pSpot /= playingPlayer
          actor :: Actor = if isAI then Move.AI else Move.Player

instance MakeHandlers 'Sim (Kernel ()) where
  make = Handlers {disableUI = id, enableUI = id, incrTurn}
    where
      -- Simulation case: do not call startTurn (AI not triggered)
      incrTurn k = k & incrTurnBase & Right <&> (,Nothing)

-- | The kind of events that happen depending on what is on the board
data EventsKind
  = -- | Events that happen at the turn start
    PreTurn
  | -- | Events that happen when pressing @End Turn@
    PreEndTurn

-- | Events to execute when the turn of the given 'Spots.Player' starts.
-- When extending this list, you likely wanna extend 'Command.Command' for
-- testing the new event.
preTurnEvents :: Spots.Player -> [Game.Event]
preTurnEvents pSpot =
  [ Game.ApplyFearNTerror $ Spots.other pSpot,
    Game.ApplyBleed pSpot,
    Game.ApplyAssassins pSpot,
    Game.ApplyBrainless pSpot,
    Game.FillTheFrontline pSpot,
    Game.ApplyGrowth pSpot,
    Game.ApplyKing pSpot
  ]

-- | Events to execute when the given 'Spots.Player' pressed @End Turn@
preEndTurnEvents :: Spots.Player -> [Game.Event]
preEndTurnEvents pSpot =
  [ Game.ApplyCreateForest pSpot
  ]

mkEvents :: EventsKind -> Shared.Model -> Turn.T -> Spots.Player -> Board.T 'Core -> [Game.Event]
mkEvents kind shared turn pSpot board =
  Game.keepEffectfull shared (Game.Playable board events turn)
  where
    events =
      case kind of
        PreTurn | Turn.initial == turn -> [] -- No pre turn events when game starts
        PreTurn -> preTurnEvents pSpot
        PreEndTurn -> preEndTurnEvents pSpot

-- | Use to avoid exposing 'EventsKind'
mkPreEndTurnEvents :: Shared.Model -> Turn.T -> Spots.Player -> Board.T 'Core -> [Game.Event]
mkPreEndTurnEvents = mkEvents PreEndTurn

data Actor = AI | Player

-- | The layer above 'startAITurn' and 'startPlayerTurn'. It intentionally
-- requires the 'Actor' AND 'Spots.Player' so that the AI can be used
-- even when there are 1 or 2 players.
startTurn :: MonadError Text.Text m => a ~ Kernel b => Actor -> Spots.Player -> a -> m (a, NextSched)
startTurn a pSpot m@Kernel {board = b, turn} =
  ( case a of
      Move.AI -> startAITurn m' pSpot
      Move.Player -> pure $ startPlayerTurn m' pSpot
  )
    <&> Bifunctor.first postStart
  where
    m'
      | turn == Turn.initial = m -- Don't increment stupidity counter for example.
      | otherwise = m {Move.board = boardStart b pSpot}

-- | This function is related to 'startAITurn'. If you change
-- this function, consider changing 'startAITurn' too.
startPlayerTurn :: a ~ Kernel b => a -> Spots.Player -> (a, NextSched)
startPlayerTurn m@Kernel {board, shared, turn} pSpot = runIdentity $ do
  (shared, board, anims) <- pure $ Game.transferCards shared board pSpot
  -- We draw the first card right away,
  -- so that the game feels responsive when the player turn starts
  let drawNow = Game.cardsToDraw board pSpot True & take 1
      preTurnEs :: [Game.Event] = mkEvents PreTurn shared turn pSpot board
  (shared, board, anims) <-
    pure $
      Game.drawCards shared board pSpot drawNow
        & (\(s, b, a) -> (s, b, a <> anims)) -- Don't forget earlier 'anims'
  let scheds = map DrawCards drawNow ++ (preTurnEs & eventsToSched & maybeToList)
  return (m `with` (shared, board, anims), scheds & toMaybe & nextify)

-- | This function is related to 'startPlayerTurn'. If you change
-- this function, consider changing 'startPlayerTurn' too.
startAITurn ::
  MonadError Text.Text m => a ~ Kernel b => a -> Spots.Player -> m (a, NextSched)
startAITurn m@Kernel {board, difficulty, shared, turn} pSpot = do
  (shared, board, anims) <- pure $ Game.transferCards shared board pSpot
  let drawSrcs :: [Game.DrawSource] = Game.cardsToDraw board pSpot True
      preTurnEs :: [Game.Event] = mkEvents PreTurn shared turn pSpot board
  (shared, board, anims) <-
    pure $
      Game.drawCards shared board pSpot drawSrcs
        & (\(s, b, a) -> (s, b, a <> anims)) -- Don't forget earlier 'anims'
  let m' = m `with` (shared, board, anims) -- Create m' now, next events will be scheduled, not executed asap
  (afterPreTurnEsShared, afterPreTurnEsBoard, _) <-
    Game.playAllE shared $ Game.Playable board preTurnEs turn -- We want 'AI.play' to be executed on the state after pre turn events
  let placeScheds :: [Sched] =
        -- Use 'after pre turn events' data for playing the AI
        AI.play difficulty afterPreTurnEsShared afterPreTurnEsBoard pSpot turn
          & map (Play . Game.PEvent)
      nextSched = (preTurnEs & eventsToSched & nextify) `cons` (placeScheds ++ [EndTurnPressed])
  pure (m', nextSched)

-- | Function to call after 'startPlayerTurn' and 'startAITurn'
postStart :: a ~ Kernel b => a -> a
postStart = id
