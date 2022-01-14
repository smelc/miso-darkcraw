{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Update where

import AI (Difficulty)
import qualified AI
import Board
import BoardInstances (boardStart)
import qualified Campaign
import Card
import Cinema
import qualified Command
import qualified Constants
import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad.Except
import qualified Data.Bifunctor as Bifunctor
import Data.Either.Extra
import Data.Foldable (asum, toList)
import Data.List.Index (setAt)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Data.TreeDiff
import qualified Data.Vector as V
import Debug.Trace
import qualified Game
import Miso
import Miso.String (MisoString, fromMisoString)
import Model
import Movie (welcomeMovie)
import Nat
import ServerMessages
import SharedModel (SharedModel)
import qualified SharedModel
import Spots hiding (Card)
import Text.Pretty.Simple
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Turn

data MultiPlayerLobbyAction
  = LobbyUpdateUsername MisoString
  | LobbySubmitUsername
  | LobbyInviteUser UserName
  | CancelInvitationClicked
  | AcceptInvitationClicked
  | RejectInvitationClicked
  | LobbyServerMessage OutMessage
  deriving (Show, Eq)

data SinglePlayerLobbyAction
  = LobbySelectTeam (Maybe Team)
  deriving (Show, Eq)

data DnDAction a
  = -- | Dragging card in hand ends. When a successful drop is done,
    -- this event is fired right after 'Drop'. We rely on that. If Drop
    -- was fired last, we would miss it. Be careful on untested browsers.
    DragEnd
  | -- | Dragging card in hand
    DragStart HandIndex
  | -- | In 'GameView', this can play the 'GamePlayEvent' 'Place'
    Drop
  | DragEnter a
  | DragLeave a
  deriving (Show, Eq)

-- | Actions that are raised by 'GameView'
data GameAction
  = -- | A drag an drop event
    GameDnD (DnDAction Game.Target)
  | -- | Play some game event. It can be an event scheduled by the AI
    -- Or a 'EndTurn' 'GamePlayEvent' scheduled because the player
    -- pressed "End Turn".
    GamePlay Game.Event
  | -- | Turn was updated previously by 'GameIncrTurn',
    -- time to draw cards from the stack. Then the handler of this event
    -- will take care of giving the player control back. This event
    -- is translated to a list of events, iteratively consuming the list.
    GameDrawCards [Game.DrawSource]
  | -- | All actions have been resolved, time to update the turn widget
    -- and to schedule 'GameDrawCard'. This does NOT translate
    -- to a 'GamePlayEvent'.
    GameIncrTurn
  | -- | End Turn button pressed in turn widget. For player, schedule
    -- attacks then 'GameIncrTurn'; for AI, compute its actions,
    -- schedule them, and then schedule attack and 'GameIncrTurn'.
    GameEndTurnPressed
  | -- | Starting hovering card in hand
    GameInHandMouseEnter HandIndex
  | -- | Ending hovering card in hand
    GameInHandMouseLeave HandIndex
  | -- | Starting hovering a target
    GameInPlaceMouseEnter Game.Target
  | -- | Ending hovering a target
    GameInPlaceMouseLeave Game.Target
  | -- | Execute a command (dev mode only)
    GameExecuteCmd
  | -- | Update the command to execute soon (dev mode only)
    GameUpdateCmd MisoString
  deriving (Show, Eq)

-- | Actions internal to 'LootView'
data LootAction
  = -- | Select a reward. The index is in the list of available rewards.
    Pick Nat
  | -- | Unselect a reward. The index is in the list of available rewards.
    Unpick Nat
  deriving (Eq, Show)

-- | To which page to go to, from the welcome page
data WelcomeDestination
  = MultiPlayerDestination
  | SinglePlayerDestination
  deriving (Eq, Show)

type DeckViewInput = [Card 'Core]

-- | Sum type for application events. If drop stuff doesn't work
-- | think whether it's affected by https://github.com/dmjio/miso/issues/478
data Action
  = -- | Leave 'DeckView', go to another view
    DeckBack
  | -- | Leave a view, go to 'DeckView'
    DeckGo DeckViewInput
  | GameAction' GameAction
  | -- | Actions internal to 'LootView'
    LootAction' LootAction
  | -- | Go to 'LootView'
    LootGo LootModel
  | NoOp
  | MultiPlayerLobbyAction' MultiPlayerLobbyAction
  | SayHelloWorld
  | SinglePlayerLobbyAction' SinglePlayerLobbyAction
  | -- Leave 'SinglePlayerView', back to 'WelcomeView'
    SinglePlayerBack
  | -- Leave 'SinglePlayerView', start game
    SinglePlayerGo
  | SceneAction' SceneAction
  | Keyboard (Set Int)
  | -- Leave 'WelcomeView', go to 'MultiPlayerView' or 'SinglePlayerView'
    WelcomeGo WelcomeDestination
  deriving (Show, Eq)

data SceneAction
  = StepScene
  | PauseOrResumeSceneForDebugging
  | JumpToFrameForDebugging Int
  deriving (Show, Eq)

logUpdates :: (Monad m, Eq a, ToExpr a) => (Action -> a -> m a) -> Action -> a -> m a
logUpdates update NoOp model = update NoOp model
logUpdates update action model = do
  model' <- update action model
  return $
    trace
      ("--------\n" ++ show action ++ "\n" ++ diff model model')
      model'
  where
    diff model model'
      | model == model' = "no diff"
      | otherwise = prettyDiff (ediff model model')
    prettyDiff edits = displayS (renderPretty 0.4 80 (ansiWlEditExprCompact edits)) ""

-- | Class defining the behavior of updating 'm' w.r.t. 'Interaction'
-- and 'DnDAction'
class Interactable m t mseq | m -> t, m -> mseq where
  considerAction :: m -> DnDAction t -> Bool

  -- | What to do when dropping card at given index, on the given 't' target
  -- The spot indicates the player whose card is being played
  drop ::
    MonadError Text.Text n =>
    m ->
    Spots.Player ->
    HandIndex ->
    t ->
    n mseq

  getPlayingPlayer :: m -> Spots.Player

  -- | When to stop a dropping action
  stopWrongDrop :: m -> Bool

  -- | The default behavior
  updateDefault :: m -> Interaction t -> mseq

  -- | How to update the abstract state 'm' with the given 'Interaction'
  withInteraction :: m -> Interaction t -> m

act ::
  MonadError Text.Text n =>
  forall m t mseq.
  Interactable m t mseq =>
  m ->
  DnDAction t ->
  Interaction t ->
  n mseq
act m a i =
  case (considerAction m a, a, i) of
    (False, _, _) -> pure $ updateDefault m NoInteraction
    (_, DragEnd, _) -> pure $ updateDefault m NoInteraction
    (_, DragStart j, _) ->
      pure $ updateDefault m $ DragInteraction $ Dragging j Nothing
    (_, Drop, DragInteraction Dragging {draggedCard, dragTarget = Just dragTarget}) ->
      Update.drop
        m
        pSpot
        draggedCard
        dragTarget
      where
        pSpot = getPlayingPlayer m
    (_, Drop, _) | stopWrongDrop m -> pure $ updateDefault m NoInteraction
    -- DragEnter cannot create a DragInteraction if there's none yet, we don't
    -- want to keep track of drag targets if a drag action did not start yet
    (_, DragEnter t, DragInteraction dragging) ->
      pure $ updateDefault m $ DragInteraction $ dragging {dragTarget = Just t}
    (_, DragLeave _, DragInteraction dragging) ->
      pure $ updateDefault m $ DragInteraction $ dragging {dragTarget = Nothing}
    _ -> pure $ updateDefault m i

instance Interactable GameModel Game.Target (GameModel, GameActionSeq) where
  considerAction m@GameModel {uiAvail} a =
    case a of
      DragEnd -> True
      DragStart _ | uiAvail && isPlayerTurn m -> True
      DragStart _ -> False
      Drop -> True
      DragEnter _ -> isPlayerTurn m
      DragLeave _ -> isPlayerTurn m

  drop m pSpot idx target = playOne m $ Game.Place pSpot target idx

  getPlayingPlayer GameModel {turn} = Turn.toPlayerSpot turn

  stopWrongDrop m = isPlayerTurn m

  updateDefault m i = (withInteraction m i, [])

  withInteraction g i = g {interaction = i}

-- | Event to fire after the given delays (in seconds). Delays add up.
type GameActionSeq = [(Int, GameAction)]

playOne ::
  MonadError Text.Text m =>
  GameModel ->
  Game.Event ->
  m (GameModel, GameActionSeq)
playOne m gamePlayEvent =
  updateGameModel m (GamePlay gamePlayEvent) NoInteraction

-- | We MUST return GameAction as the second element (as opposed to
-- 'GamePlayEvent'). This is used for 'GameIncrTurn' for example.
-- The second element must only contain delayed stuff, it's invalid
-- to return a list whose first element has 0. If we did that, we would
-- need to play this event right away, and then we would have new delayed
-- actions to consider which we wouldn't know how to merge with the first ones.
-- If you feel like adding a 0 event, you instead need to play this event
-- right away by doing a recursive call (or use 'playOne')
updateGameModel ::
  MonadError Text.Text m =>
  GameModel ->
  GameAction ->
  Interaction Game.Target ->
  m (GameModel, GameActionSeq)
-- This is the only definition that should care about GameShowErrorInteraction:
updateGameModel m action (ShowErrorInteraction _) =
  updateGameModel m action NoInteraction -- clear error message
  -- Now onto "normal" stuff:
updateGameModel m (GameDnD a@DragEnd) i = act m a i
updateGameModel m (GameDnD a@(DragStart _)) i = act m a i
updateGameModel m (GameDnD a@Drop) i = act m a i
updateGameModel m (GameDnD a@(DragEnter _)) i = act m a i
updateGameModel m (GameDnD a@(DragLeave _)) i = act m a i
-- A GamePlayEvent to execute
updateGameModel m@GameModel {board, shared} (GamePlay gameEvent) _ = do
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
              (True, Just cSpot') -> Just $ GamePlay $ Game.Attack pSpot cSpot' True changeTurn
            where
              terminator = if changeTurn then Just GameIncrTurn else Nothing
          (Game.Attack {}, Just e) ->
            error $ "Cannot mix Game.Attack and events when enqueueing but got event: " ++ show e
          (_, _) -> GamePlay <$> generated
  -- There MUST be a delay here, otherwise it means we would need
  -- to execute this event now. We don't want that. 'playAll' checks that.
  pure $ (m', zip (repeat 1) $ maybeToList $ nextEvent)
updateGameModel m (GameDrawCards []) _ =
  pure $ traceShow ("GameDrawCards [] should not happen (but is harmless)" :: String) (m, [])
updateGameModel m@GameModel {board, shared, turn} (GameDrawCards (fst : rest)) _ = do
  (shared', board', boardui') <- Game.drawCardE shared board pSpot fst
  pure $
    ( m {anims = boardui', board = board', shared = shared'},
      -- enqueue next event (if any)
      [(1, GameDrawCards rest) | not $ null rest]
    )
  where
    pSpot = Turn.toPlayerSpot turn
-- "End Turn" button pressed by the player or the AI
updateGameModel m@GameModel {board, difficulty, playingPlayer, shared, turn} GameEndTurnPressed _ = do
  m@GameModel {board} <-
    ( if isInitialTurn
        then do
          -- End Turn pressed at the end of the player's first turn, make the AI
          -- place its card in a state where the player did not put its
          -- card yet, then place them all at once; and then continue
          -- Do not reveal player placement to AI
          let emptyPlayerInPlaceBoard = Board.setInPlace board pSpot Map.empty
              placements =
                AI.placeCards
                  difficulty
                  shared
                  emptyPlayerInPlaceBoard
                  (turn & Turn.next & Turn.toPlayerSpot)
          (shared, board, anims) <- Game.playAllE shared board placements
          pure $ m {anims, board, shared}
        else pure m
      )
      <&> disableUI
  let event = mkEvent board
  if isInitialTurn
    then -- We want a one second delay, to see clearly that the opponent
    -- puts its cards, and then proceed with resolving attacks
      pure $ (m, [(1, event)])
    else -- We don't want any delay so that the game feels responsive
    -- when the player presses "End Turn", hence the recursive call.
      updateGameModel m (event) NoInteraction
  where
    pSpot = Turn.toPlayerSpot turn
    isInitialTurn = turn == Turn.initial
    disableUI gm = if pSpot == playingPlayer then gm {uiAvail = False} else gm
    mkEvent b =
      -- schedule resolving first attack
      case Game.nextAttackSpot b pSpot Nothing of
        Nothing -> GameIncrTurn -- no attack, change turn right away
        Just cSpot -> GamePlay $ Game.Attack pSpot cSpot True True
updateGameModel m GameIncrTurn _ = do
  (m, seq) <- updateGameIncrTurn m
  pure (enableUI m, seq)
  where
    enableUI gm@GameModel {playingPlayer, turn}
      | Turn.toPlayerSpot turn == playingPlayer =
        gm {uiAvail = True} -- Restore interactions if turn of player
      | otherwise = gm
-- Hovering in hand cards
updateGameModel m (GameInHandMouseEnter i) NoInteraction =
  pure $ updateDefault m $ HoverInteraction $ Hovering i
updateGameModel m (GameInHandMouseLeave _) _ =
  pure $ updateDefault m NoInteraction
-- Hovering in place cards
updateGameModel m (GameInPlaceMouseEnter target) NoInteraction =
  pure $ updateDefault m $ HoverInPlaceInteraction target
updateGameModel m (GameInPlaceMouseLeave _) _ =
  pure $ updateDefault m NoInteraction
-- Debug cmd
updateGameModel m GameExecuteCmd _ =
  pure $
    updateDefault m $
      ShowErrorInteraction "GameExecuteCmd should be handled in updateModel, because it can change page"
updateGameModel m@GameModel {shared} (GameUpdateCmd misoStr) i =
  pure $
    updateDefault
      ((m {shared = SharedModel.withCmd shared (Just $ fromMisoString misoStr)}) :: GameModel)
      i
-- default
updateGameModel m _ i =
  pure $ updateDefault m i

updateGameIncrTurn ::
  MonadError Text.Text m =>
  GameModel ->
  m (GameModel, GameActionSeq)
updateGameIncrTurn m@GameModel {board, difficulty, playingPlayer, shared, turn} = do
  board <- pure $ boardStart board pSpot
  (shared, board, anims) <- pure $ Game.transferCards shared board pSpot
  let drawSrcs = Game.cardsToDraw board pSpot True
      -- If it's the player turn, we wanna draw the first card right away,
      -- so that the game feels responsive.
      drawNow = if isAI then drawSrcs else take 1 drawSrcs
  (shared, board, anims) <-
    Game.drawCardsE shared board pSpot drawNow
      <&> (\(s, b, a) -> (s, b, a <> anims)) -- Don't forget earlier 'anims'
  let preTurnEvents =
        Game.keepEffectfull
          shared
          board
          [ Game.ApplyFearNTerror otherSpot,
            Game.ApplyBrainless pSpot,
            Game.FillTheFrontline pSpot,
            Game.ApplyKing pSpot
          ]
  let events =
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
            let plays = AI.play difficulty shared board' pSpot
             in zip (repeat 1) (map GamePlay plays ++ [GameEndTurnPressed])
          (False, _) ->
            let drawSoon = Prelude.drop 1 drawSrcs
             in [(1, GameDrawCards drawSoon) | not $ null drawSoon]
  m <- pure $ m {anims, board, shared, turn = turn'}
  pure $ (m, [(1, GamePlay event) | event <- preTurnEvents] ++ events)
  where
    turn' = Turn.next turn
    pSpot = Turn.toPlayerSpot turn'
    otherSpot = otherPlayerSpot pSpot
    isAI = pSpot /= playingPlayer

-- | Update a 'LootModel' according to the input 'LootAction'
updateLootModel :: LootAction -> LootModel -> LootModel
updateLootModel action lm@LootModel {rewards = pairs} =
  lm {rewards}
  where
    rewards =
      case action of
        Pick n -> set n Model.Picked
        Unpick n -> set n Model.NotPicked
    set n value =
      setAt (natToInt n) (card, value) pairs
      where
        card = pairs !! (natToInt n) & fst

updateSinglePlayerLobbyModel ::
  SinglePlayerLobbyAction ->
  SinglePlayerLobbyModel ->
  SinglePlayerLobbyModel
updateSinglePlayerLobbyModel (LobbySelectTeam t) m =
  m {singlePlayerLobbyTeam = t}

-- | Updates a 'MultiPlayerLobbyModel'
updateMultiPlayerLobbyModel :: MultiPlayerLobbyAction -> MultiPlayerLobbyModel -> Effect Action MultiPlayerLobbyModel
updateMultiPlayerLobbyModel
  (LobbyUpdateUsername userName)
  (CollectingUserName _) =
    noEff $ CollectingUserName (fromMisoString userName)
updateMultiPlayerLobbyModel
  LobbySubmitUsername
  (CollectingUserName userName) =
    WaitingForNameSubmission userName <# do
      Miso.send (CreateUser userName)
      return NoOp
updateMultiPlayerLobbyModel
  (LobbyServerMessage UserCreated)
  (WaitingForNameSubmission userName) =
    noEff $ DisplayingUserList Nothing userName []
updateMultiPlayerLobbyModel
  (LobbyServerMessage (NewUserList users))
  (DisplayingUserList merror userName _) =
    noEff $ DisplayingUserList merror userName users
updateMultiPlayerLobbyModel
  (LobbyServerMessage (NewUserList users))
  (InvitingUser me _ user state) =
    noEff $ InvitingUser me users user state
updateMultiPlayerLobbyModel
  (LobbyServerMessage (NewUserList users))
  (Invited me _ user state) =
    noEff $ Invited me users user state
updateMultiPlayerLobbyModel
  (LobbyInviteUser user)
  (DisplayingUserList _ me users) =
    InvitingUser me users user WaitingForUserInvitationAck <# do
      Miso.send (InviteUser user)
      return NoOp
updateMultiPlayerLobbyModel
  (LobbyServerMessage UserBusy)
  (InvitingUser me users user WaitingForUserInvitationAck) =
    noEff $ DisplayingUserList (Just (UserBusyError user)) me users
updateMultiPlayerLobbyModel
  (LobbyServerMessage InvitationSent)
  (InvitingUser me users user WaitingForUserInvitationAck) =
    noEff $ InvitingUser me users user WaitingForRSVP
updateMultiPlayerLobbyModel
  CancelInvitationClicked
  (InvitingUser me users user WaitingForRSVP) =
    InvitingUser me users user WaitingForInvitationDropAck <# do
      Miso.send DropInvitation
      return NoOp
updateMultiPlayerLobbyModel
  (LobbyServerMessage InvitationDropAck)
  (InvitingUser me users _ WaitingForInvitationDropAck) =
    noEff $ DisplayingUserList Nothing me users
updateMultiPlayerLobbyModel
  (LobbyServerMessage InvitationRejected)
  (InvitingUser me users user WaitingForRSVP) =
    noEff $ DisplayingUserList (Just (InvitationRejectedError user)) me users
updateMultiPlayerLobbyModel
  (LobbyServerMessage InvitationAccepted)
  (InvitingUser me _ user WaitingForRSVP) =
    noEff $ GameStarted me user
updateMultiPlayerLobbyModel
  (LobbyServerMessage (IncomingInvitation user))
  (DisplayingUserList _ me users) =
    noEff $ Invited me users user CollectingUserRSVP
updateMultiPlayerLobbyModel
  (LobbyServerMessage IncomingInvitationCancelled)
  (Invited me users user _) =
    noEff $ DisplayingUserList (Just (InvitationCancelledError user)) me users
updateMultiPlayerLobbyModel
  RejectInvitationClicked
  (Invited me users user CollectingUserRSVP) =
    Invited me users user WaitingForRejectionAck <# do
      Miso.send RejectInvitation
      return NoOp
updateMultiPlayerLobbyModel
  (LobbyServerMessage IncomingInvitationRejectionAck)
  (Invited me users _ WaitingForRejectionAck) =
    noEff $ DisplayingUserList Nothing me users
updateMultiPlayerLobbyModel
  AcceptInvitationClicked
  (Invited me users user CollectingUserRSVP) =
    Invited me users user WaitingForAcceptanceAck <# do
      Miso.send AcceptInvitation
      return NoOp
updateMultiPlayerLobbyModel
  (LobbyServerMessage IncomingInvitationAcceptanceAck)
  (Invited me _ user WaitingForAcceptanceAck) =
    noEff $ GameStarted me user
updateMultiPlayerLobbyModel _ m =
  noEff m

-- Function courtesy of @dmjio!
delayActions :: m -> [(Int, Action)] -> Effect Action m
delayActions m actions =
  Effect
    m
    [ \sink -> do
        liftIO $ threadDelay i
        liftIO (sink action)
      | (i, action) <- actions
    ]

toSceneModel :: Scene () -> SceneModel
toSceneModel scene = SceneNotStarted (V.fromList (Cinema.render scene))

-- | Seconds to scheduling delay
toSecs :: Int -> Int
toSecs x = x * 1000000

-- | Tenth of seconds to scheduling delay
tenthToSecs :: Int -> Int
tenthToSecs x = x * 100000

updateSceneModel :: SceneAction -> SceneModel -> Effect Action SceneModel
updateSceneModel StepScene sceneModel =
  case sceneModel of
    SceneNotStarted frames
      | V.null frames -> noEff $ SceneComplete frames
      | otherwise -> playFrame frames 0
    ScenePlaying frames i
      | i < length frames - 1 -> playFrame frames (i + 1)
      | otherwise -> noEff $ SceneComplete frames
    completeOrPaused -> noEff completeOrPaused
  where
    playFrame frames i =
      let TimedFrame {duration} = frames V.! i
       in delayActions
            (ScenePlaying frames i)
            [(tenthToSecs duration, SceneAction' StepScene)]
updateSceneModel PauseOrResumeSceneForDebugging sceneModel =
  case sceneModel of
    SceneNotStarted frames ->
      noEff $ ScenePausedForDebugging frames 0
    ScenePlaying frames i ->
      noEff $ ScenePausedForDebugging frames i
    SceneComplete frames ->
      noEff $ ScenePausedForDebugging frames (length frames - 1)
    ScenePausedForDebugging frames i ->
      ScenePlaying frames i <# return (SceneAction' StepScene)
updateSceneModel (JumpToFrameForDebugging i) sceneModel =
  case sceneModel of
    SceneNotStarted frames
      | indexWithinBounds frames ->
        noEff $ ScenePausedForDebugging frames i
    SceneComplete frames
      | indexWithinBounds frames ->
        noEff $ ScenePausedForDebugging frames i
    ScenePausedForDebugging frames _
      | indexWithinBounds frames ->
        noEff $ ScenePausedForDebugging frames i
    anyOtherState ->
      noEff anyOtherState
  where
    indexWithinBounds frames = i >= 0 && i < length frames

-- | Updates model, optionally introduces side effects
-- | This function delegates to the various specialized functions
-- | and is the only one to handle page changes. A page change event
-- | is one that takes a model from a page and returns a model of another page.
-- | Such an event cannot be dealt with a specialized function
-- | (SpecializedAction -> SpecializedModel -> SpecializedModel),
-- | it needs to be in `Action -> Model -> Model`.
updateModel :: Action -> Model -> Effect Action Model
-- Generic actions
updateModel NoOp m = noEff m
updateModel SayHelloWorld m =
  -- Say hello and start scene stepping
  m <# do consoleLog "miso-darkcraw says hello" >> pure (SceneAction' StepScene)
-- Actions that change the page
-- Leave 'DeckView'
updateModel DeckBack (DeckModel' DeckModel {..}) =
  noEff deckBack
-- Leave 'GameView', go to 'DeckView'
updateModel (DeckGo deck) m@(GameModel' GameModel {..}) =
  noEff $ DeckModel' $ DeckModel deck m playingPlayer t shared
  where
    t = Board.toPart board playingPlayer & Board.team
-- Go to 'LootView'
updateModel (LootGo model) _ =
  noEff $ LootModel' model
-- Schedule leaving 'GameView', to go to 'LootView'
updateModel _ m@(GameModel' gm@GameModel {board, level, playingPlayer, turn})
  | (Turn.next turn & Turn.toNat) > Constants.nbTurns =
    case Campaign.succ level of
      Nothing -> noEff m -- TODO, go to global victory view
      Just _ ->
        delayActions m' [(toSecs 1, LootGo $ Model.endGame gm outcome)]
        where
          m' = GameModel' gm {anim = Game.Fadeout}
          part = Board.toPart board playingPlayer
          (score, enemy) =
            (Board.score part, Board.toPart board (otherPlayerSpot playingPlayer) & Board.score)
          outcome :: Campaign.Outcome =
            case compare score enemy of
              LT -> Campaign.Loss
              EQ -> Campaign.Draw
              GT -> Campaign.Win
-- Leave 'GameView' (maybe)
updateModel (GameAction' GameExecuteCmd) (GameModel' gm@GameModel {board, shared, playingPlayer})
  | SharedModel.getCmd shared & isJust =
    let cmdStr = SharedModel.getCmd shared
     in case cmdStr <&> Command.read & join of
          Nothing ->
            let errMsg = "Unrecognized command: " ++ show cmdStr
             in noEff $ GameModel' $ gm {interaction = ShowErrorInteraction $ Text.pack errMsg}
          Just (Command.EndGame outcome) ->
            noEff $ LootModel' $ Model.endGame gm outcome
          Just (Command.FillTheFrontline pSpot) ->
            playEvent Game.FillTheFrontline pSpot
          Just (Command.Gimme cid) ->
            withBoard $ Board.addToHand board playingPlayer cid
          Just (Command.GimmeMana) ->
            let mana = Board.toPart board playingPlayer & Board.mana
             in withBoard $ Board.setMana (mana + 1) playingPlayer board
          Just (Command.HailToTheKing pSpot) ->
            playEvent Game.ApplyKing pSpot
          Just (Command.Killall pSpot) ->
            withBoard $ Board.setPart board pSpot (part {inPlace = mempty})
            where
              part = Board.toPart board pSpot
          Just (Command.Reboot pSpot team) ->
            withBoard $ Board.setPart board pSpot part
            where
              (inHand, stack) =
                SharedModel.getInitialDeck shared team
                  & map Card.cardToIdentifier
                  & splitAt Constants.initialHandSize
              part = (Board.empty team) {inHand, stack}
  where
    withBoard board' = noEff $ GameModel' $ gm {board = board'}
    playEvent eventMaker pSpot =
      updateModel
        (GameAction' $ GamePlay $ eventMaker pSpot)
        (GameModel' gm)
-- Actions that leave 'SinglePlayerView'
updateModel
  SinglePlayerBack
  (SinglePlayerLobbyModel' SinglePlayerLobbyModel {..}) =
    noEff $ WelcomeModel' $ initialWelcomeModel singlePlayerLobbyShared
updateModel
  SinglePlayerGo
  ( SinglePlayerLobbyModel'
      SinglePlayerLobbyModel
        { singlePlayerLobbyTeam = Just team,
          singlePlayerLobbyShared = shared
        }
    ) =
    noEff $ GameModel' $ level0GameModel AI.Hard shared $ Teams Undead team
-- Actions that leave 'WelcomeView'
updateModel
  (WelcomeGo SinglePlayerDestination)
  (WelcomeModel' WelcomeModel {shared}) =
    noEff $ SinglePlayerLobbyModel' $ SinglePlayerLobbyModel Nothing shared
updateModel (WelcomeGo MultiPlayerDestination) (WelcomeModel' _) =
  effectSub (MultiPlayerLobbyModel' (CollectingUserName "")) $
    websocketSub (URL "ws://127.0.0.1:9160") (Protocols []) handleWebSocket
  where
    handleWebSocket (WebSocketMessage action) = MultiPlayerLobbyAction' (LobbyServerMessage action)
    handleWebSocket problem = traceShow problem NoOp
-- Actions that do not change the page delegate to more specialized versions
updateModel (SceneAction' action) (WelcomeModel' wm@WelcomeModel {sceneModel}) = do
  newSceneModel <- updateSceneModel action sceneModel
  return (WelcomeModel' wm {sceneModel = newSceneModel})
updateModel (SceneAction' _) model = noEff model
updateModel (Keyboard newKeysDown) (WelcomeModel' wm@WelcomeModel {keysDown, sceneModel}) = do
  newSceneModel <- maybe (return sceneModel) (`updateSceneModel` sceneModel) sceneAction
  return $ WelcomeModel' wm {keysDown = newKeysDown, sceneModel = newSceneModel}
  where
    sceneAction :: Maybe SceneAction
    sceneAction = asum (map keyCodeToSceneAction (toList (Set.difference newKeysDown keysDown)))
    keyCodeToSceneAction :: Int -> Maybe SceneAction
    keyCodeToSceneAction 80 = Just PauseOrResumeSceneForDebugging -- P key
    keyCodeToSceneAction _ = Nothing
updateModel (Keyboard _) model = noEff model
updateModel (GameAction' a) (GameModel' m@GameModel {interaction}) =
  if null actions
    then noEff m''
    else delayActions m'' $ prepare $ check actions
  where
    -- 'Game.NoAnimation': clear animation if any
    (m', actions) =
      either
        (\errMsg -> (withInteraction m (ShowErrorInteraction errMsg), [])) -- Show message if error occurred
        id -- return obtained model if no error
        (runExcept $ updateGameModel (m {anim = Game.NoAnimation}) a interaction)
    m'' = GameModel' m'
    sumDelays _ [] = []
    sumDelays d ((i, a) : tl) = (d + i, a) : sumDelays (d + i) tl
    prepare as = map (Bifunctor.bimap toSecs GameAction') $ sumDelays 0 as
    check ((0, event) : _) = error $ "updateGameModel should not return event with 0 delay, but " ++ show event ++ " did"
    check as = as
updateModel (LootAction' a) (LootModel' m) =
  noEff $ LootModel' $ updateLootModel a m
updateModel (SinglePlayerLobbyAction' a) (SinglePlayerLobbyModel' m) =
  noEff $ SinglePlayerLobbyModel' $ updateSinglePlayerLobbyModel a m
updateModel (MultiPlayerLobbyAction' a) (MultiPlayerLobbyModel' m) =
  MultiPlayerLobbyModel' `fmap` updateMultiPlayerLobbyModel a m
updateModel a m =
  error $
    LazyText.unpack $
      "Unhandled case in updateModel with the model being:\n"
        <> pShowNoColor m
        <> "\nand the action being:\n"
        <> pShowNoColor a

-- | The initial model, appropriately shuffled with 'SharedModel' rng
level0GameModel ::
  Difficulty ->
  SharedModel ->
  (Teams Team) ->
  GameModel
level0GameModel difficulty shared teams =
  levelNGameModel
    difficulty
    shared
    Campaign.Level0
    $ (teams <&> (\t -> (t, SharedModel.getInitialDeck shared t)))

-- | A model that takes the decks as parameters, for use after the initial
-- start of the game
levelNGameModel ::
  Difficulty ->
  SharedModel ->
  Campaign.Level ->
  -- | The initial decks
  (Teams (Team, [Card 'Core])) ->
  GameModel
levelNGameModel difficulty shared level teams =
  GameModel {..}
  where
    (_, board) = Board.initial shared teams
    interaction = NoInteraction
    playingPlayer = startingPlayerSpot
    playingPlayerDeck =
      toData playingPlayer teams
        & snd
        & map Card.cardToIdentifier
    turn = Turn.initial
    anims = mempty
    anim = Game.NoAnimation
    uiAvail = True

-- | An initial model, with a custom board
unsafeInitialGameModel ::
  Difficulty ->
  SharedModel ->
  -- | The initial decks
  (Teams (Team, [Card 'Core])) ->
  -- | The board
  Board 'Core ->
  GameModel
unsafeInitialGameModel difficulty shared teamsData board =
  GameModel {..}
  where
    interaction = NoInteraction
    level = Campaign.Level0
    playingPlayer = startingPlayerSpot
    playingPlayerDeck =
      toData playingPlayer teamsData
        & snd
        & map Card.cardToIdentifier
    turn = Turn.initial
    anims = mempty
    anim = Game.NoAnimation
    uiAvail = True

initialWelcomeModel :: SharedModel -> WelcomeModel
initialWelcomeModel shared =
  WelcomeModel
    { sceneModel = toSceneModel Movie.welcomeMovie,
      keysDown = mempty,
      ..
    }
