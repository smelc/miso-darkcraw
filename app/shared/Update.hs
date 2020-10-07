{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Update where

import AI (aiPlay)
import Board
import Card
import Cinema (ActorState, Direction, DirectionChange, Element, Frame, Scene (..), SpriteChange, TellingChange, TimedFrame (..), render)
import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad.Except (runExcept)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Bifunctor as DataBifunctor
import Data.Foldable (asum, toList)
import Data.Function ((&))
import Data.Maybe (maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.Lazy as Text
import Data.TreeDiff
import qualified Data.Vector as V
import Debug.Trace
import Game (GamePlayEvent (..), nextAttackSpot)
import qualified Game
import Miso
import Miso.String (MisoString, fromMisoString)
import Model
import Movie (welcomeMovie)
import ServerMessages
import SharedModel (SharedModel (..))
import System.Random (StdGen)
import Text.Pretty.Simple
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Tile (Tile, TileUI)
import Turn (Turn, initialTurn, nextTurn, turnToPlayerSpot)

instance ToExpr CreatureKind

instance ToExpr Team

instance ToExpr CreatureID

instance ToExpr Skill

instance ToExpr Neutral

instance ToExpr Item

instance ToExpr Filepath

instance ToExpr (Creature Core)

instance ToExpr (Creature UI)

instance ToExpr (Card Core)

instance ToExpr (Card UI)

instance ToExpr PlayerSpot

instance ToExpr AttackEffect

instance ToExpr AttackEffects

instance ToExpr CardIdentifier

instance ToExpr (PlayerPart Core)

instance ToExpr (PlayerPart UI)

instance ToExpr CardSpot

instance ToExpr (Board Core)

instance ToExpr (Board UI)

instance ToExpr HandIndex

instance ToExpr HandFiddle

instance ToExpr Dragging

instance ToExpr Hovering

instance ToExpr GameInteraction

instance ToExpr Turn

instance ToExpr StdGen where toExpr = defaultExprViaShow

instance ToExpr Tile.TileUI

instance ToExpr SharedModel

instance ToExpr GameModel

instance ToExpr PlayingMode

instance ToExpr SinglePlayerLobbyModel

instance ToExpr Cinema.SpriteChange

instance ToExpr Cinema.Direction

instance ToExpr Cinema.ActorState

instance ToExpr Tile.Tile

instance ToExpr Cinema.Element

instance ToExpr a => ToExpr (TimedFrame a)

instance ToExpr Cinema.DirectionChange

instance ToExpr Cinema.TellingChange

instance ToExpr a => ToExpr (Cinema.Frame a)

instance ToExpr SceneModel

instance ToExpr WelcomeModel

instance ToExpr MultiPlayerLobbyModel

instance ToExpr MultiPlayerLobbyError

instance ToExpr InvitationActorState

instance ToExpr InvitedActorState

instance ToExpr DeckModel

instance ToExpr Model

-- FIXME smelc move Action* to its own file (to avoid cycles later on)

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

-- | Actions that are raised by 'GameView'
data GameAction
  = -- | Play some game event. It can be an event scheduled by the AI
    -- | Or a 'EndTurn' 'GamePlayEvent' scheduled because the player
    -- | pressed "End Turn".
    GamePlay GamePlayEvent
  | -- | All actions have been resolved, time to update the turn widget
    -- | and give the next player the control. This does NOT translate
    -- | to a 'GamePlayEvent'.
    GameIncrTurn
  | -- | Dragging card in hand
    GameDragStart HandIndex
  | -- | This can play the 'GamePlayEvent' 'Place'
    GameDrop
  | GameDragEnter CardSpot
  | GameDragLeave CardSpot
  | -- | End Turn button pressed in turn widget. For player, schedule
    -- | attacks then 'GameIncrTurn'; for AI, compute its actions,
    -- | schedule them, and then schedule attack and 'GameIncrTurn'.
    GameEndTurnPressed
  | -- | Starting hovering card in hand
    GameInHandMouseEnter HandIndex
  | -- | Ending hovering card in hand
    GameInHandMouseLeave HandIndex
  | -- | Starting hovering card in place
    GameInPlaceMouseEnter PlayerSpot CardSpot
  | -- | Ending hovering card in place
    GameInPlaceMouseLeave PlayerSpot CardSpot
  deriving (Show, Eq)

-- | To which page to go to, from the welcome page
data WelcomeDestination
  = MultiPlayerDestination
  | SinglePlayerDestination
  deriving (Eq, Show)

type DeckViewInput = [Card Core]

-- | Sum type for application events. If drop stuff doesn't work
-- | think whether it's affected by https://github.com/dmjio/miso/issues/478
data Action
  = -- | Leave 'DeckView', go to another view
    DeckBack
  | -- | Leave a view, go to 'DeckView'
    DeckGo DeckViewInput
  | GameAction' GameAction
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

-- | Event to fire after the given delays (in seconds). Delays add up.
type GameActionSeq = [(Int, GameAction)]

withInteraction :: GameModel -> GameInteraction -> (GameModel, GameActionSeq)
withInteraction m i = (m {interaction = i}, [])

playOne :: GameModel -> GamePlayEvent -> (GameModel, GameActionSeq)
playOne m gamePlayEvent =
  updateGameModel m (GamePlay gamePlayEvent) GameNoInteraction

-- | We MUST return GameAction as the second element (as opposed to
-- | 'GamePlayEvent'). This is used for 'GameIncrTurn' for example.
-- | The second element must only contain delayed stuff, it's invalid
-- | to return a list whose first element has 0. If we did that, we would
-- | need to play this event right away, and then we would have new delayed
-- | actions to consider which we wouldn't know how to merge with the first ones.
-- | If you feel like adding a 0 event, you instead need to play this event
-- | right away by doing a recursive call (or using 'playOne')
updateGameModel ::
  GameModel ->
  GameAction ->
  GameInteraction ->
  (GameModel, GameActionSeq)
-- This is the only definition that should care about GameShowErrorInteraction:
updateGameModel m action (GameShowErrorInteraction _) =
  updateGameModel m action GameNoInteraction -- clear error message
  -- Now onto "normal" stuff:
updateGameModel m (GameDragStart i) _
  | isPlayerTurn m =
    withInteraction m $ GameDragInteraction $ Dragging i Nothing
updateGameModel
  m@GameModel {playingPlayer}
  GameDrop
  (GameDragInteraction Dragging {draggedCard, dragTarget = Just dragTarget}) =
    playOne m $ Place playingPlayer dragTarget draggedCard
updateGameModel m GameDrop _
  | isPlayerTurn m =
    withInteraction m GameNoInteraction
-- DragEnter cannot create a DragInteraction if there's none yet, we don't
-- want to keep track of drag targets if a drag action did not start yet
updateGameModel m (GameDragEnter cSpot) (GameDragInteraction dragging)
  | isPlayerTurn m =
    withInteraction m $ GameDragInteraction $ dragging {dragTarget = Just cSpot}
updateGameModel m (GameDragLeave _) (GameDragInteraction dragging)
  | isPlayerTurn m =
    withInteraction m $ GameDragInteraction $ dragging {dragTarget = Nothing}
-- A GamePlayEvent to execute
updateGameModel m@GameModel {board} (GamePlay gameEvent) _ =
  case Game.play board gameEvent of
    Left errMsg -> withInteraction m $ GameShowErrorInteraction errMsg
    Right (board', anims') ->
      -- There MUST be a delay here, otherwise it means we would need
      -- to execute this event now. We don't want that. 'playAll' checks that.
      (m', zip (repeat 1) $ maybeToList event)
      where
        m' = m {board = board', anims = anims'}
        event = case gameEvent of
          EndTurn pSpot cSpot ->
            -- enqueue resolving next attack if applicable
            case nextAttackSpot board pSpot (Just cSpot) of
              Nothing -> Just GameIncrTurn -- no more attack, change turn
              Just cSpot' -> Just $ GamePlay $ EndTurn pSpot cSpot'
          _ -> Nothing
-- "End Turn" button pressed by the player or the AI
updateGameModel m@GameModel {board, turn} GameEndTurnPressed _ =
  -- We don't want any delay so that the game feels responsive
  -- when the player presses "End Turn", hence the recursive call.
  updateGameModel m' event GameNoInteraction
  where
    m' = m {interaction = GameNoInteraction}
    pSpot = turnToPlayerSpot turn
    event =
      -- schedule resolving first attack
      case nextAttackSpot board pSpot Nothing of
        Nothing -> GameIncrTurn -- no more attack, change turn
        Just cSpot -> GamePlay $ EndTurn pSpot cSpot
updateGameModel m@GameModel {board, playingPlayer, turn} GameIncrTurn _ =
  (m'', events)
  where
    turn' = nextTurn turn
    m' = m {turn = turn'}
    (m'', events) =
      if turnToPlayerSpot turn' == playingPlayer
        then (m', [])
        else -- next turn is AI
        case aiPlay board turn' & runExcept of
          Left errMsg -> withInteraction m' $ GameShowErrorInteraction errMsg
          Right events ->
            -- schedule its actions, then simulate pressing "End Turn"
            (m', snoc events' (1, GameEndTurnPressed))
            where
              -- We want a one second delay, it's make it easier to understand
              -- what's going on
              events' = zip (repeat 1) $ map GamePlay events
-- Hovering in hand cards
updateGameModel m (GameInHandMouseEnter i) GameNoInteraction =
  withInteraction m $ GameHoverInteraction $ Hovering i
updateGameModel m (GameInHandMouseLeave _) _ =
  withInteraction m GameNoInteraction
-- Hovering in place cards
updateGameModel m (GameInPlaceMouseEnter pSpot cSpot) GameNoInteraction =
  withInteraction m $ GameHoverInPlaceInteraction pSpot cSpot
updateGameModel m (GameInPlaceMouseLeave _ _) _ =
  withInteraction m GameNoInteraction
-- default
updateGameModel m _ i =
  withInteraction m i

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
      send (CreateUser userName)
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
      send (InviteUser user)
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
      send DropInvitation
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
      send RejectInvitation
      return NoOp
updateMultiPlayerLobbyModel
  (LobbyServerMessage IncomingInvitationRejectionAck)
  (Invited me users _ WaitingForRejectionAck) =
    noEff $ DisplayingUserList Nothing me users
updateMultiPlayerLobbyModel
  AcceptInvitationClicked
  (Invited me users user CollectingUserRSVP) =
    Invited me users user WaitingForAcceptanceAck <# do
      send AcceptInvitation
      return NoOp
updateMultiPlayerLobbyModel
  (LobbyServerMessage IncomingInvitationAcceptanceAck)
  (Invited me _ user WaitingForAcceptanceAck) =
    noEff $ GameStarted me user
updateMultiPlayerLobbyModel a m =
  noEff $ traceShow (a, m) m

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
updateModel action _ | traceShow action False = undefined
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
  noEff $ DeckModel' $ DeckModel deck m playingPlayer gameShared
-- Actions that leave 'SinglePlayerView'
updateModel
  SinglePlayerBack
  (SinglePlayerLobbyModel' SinglePlayerLobbyModel {..}) =
    noEff $ WelcomeModel' $ initialWelcomeModel singlePlayerLobbyShared
updateModel
  SinglePlayerGo
  ( SinglePlayerLobbyModel'
      SinglePlayerLobbyModel
        { singlePlayerLobbyTeam = Just _,
          singlePlayerLobbyShared = shared
        }
    ) =
    noEff $ GameModel' $ initialGameModel shared
-- Actions that leave 'WelcomeView'
updateModel
  (WelcomeGo SinglePlayerDestination)
  (WelcomeModel' WelcomeModel {welcomeShared}) =
    noEff $ SinglePlayerLobbyModel' $ SinglePlayerLobbyModel Nothing welcomeShared
updateModel (WelcomeGo MultiPlayerDestination) (WelcomeModel' _) =
  effectSub (MultiPlayerLobbyModel' (CollectingUserName "")) $
    websocketSub (URL "ws://127.0.0.1:9160") (Protocols []) handleWebSocket
  where
    handleWebSocket (WebSocketMessage action) = MultiPlayerLobbyAction' (LobbyServerMessage action)
    handleWebSocket problem = traceShow problem NoOp
-- Actions that do not change the page delegate to more specialized versions
updateModel (SceneAction' action) (WelcomeModel' wm@WelcomeModel {welcomeSceneModel}) = do
  newWelcomeSceneModel <- updateSceneModel action welcomeSceneModel
  return (WelcomeModel' wm {welcomeSceneModel = newWelcomeSceneModel})
updateModel (SceneAction' _) model = noEff model
updateModel (Keyboard newKeysDown) (WelcomeModel' wm@WelcomeModel {keysDown, welcomeSceneModel}) = do
  newWelcomeSceneModel <- maybe (return welcomeSceneModel) (`updateSceneModel` welcomeSceneModel) sceneAction
  return $ WelcomeModel' wm {keysDown = newKeysDown, welcomeSceneModel = newWelcomeSceneModel}
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
    (m', actions) = updateGameModel m a interaction
    m'' = GameModel' m'
    sumDelays _ [] = []
    sumDelays d ((i, a) : tl) = (d + i, a) : sumDelays (d + i) tl
    prepare as = map (DataBifunctor.bimap toSecs GameAction') $ sumDelays 0 as
    check ((0, event) : _) = error $ "updateGameModel should not return event with 0 delay, but " ++ show event ++ " did"
    check as = as
updateModel (SinglePlayerLobbyAction' a) (SinglePlayerLobbyModel' m) =
  noEff $ SinglePlayerLobbyModel' $ updateSinglePlayerLobbyModel a m
updateModel (MultiPlayerLobbyAction' a) (MultiPlayerLobbyModel' m) =
  MultiPlayerLobbyModel' `fmap` updateMultiPlayerLobbyModel a m
updateModel a m =
  error $ Text.unpack $
    "Unhandled case in updateModel with the model being:\n"
      <> pShowNoColor m
      <> "\nand the action being:\n"
      <> pShowNoColor a

initialGameModel :: SharedModel -> GameModel
initialGameModel shared =
  GameModel
    shared
    board
    GameNoInteraction
    startingPlayerSpot
    initialTurn
    mempty
  where
    board = exampleBoard $ sharedCards shared

initialWelcomeModel :: SharedModel -> WelcomeModel
initialWelcomeModel welcomeShared =
  WelcomeModel
    { welcomeSceneModel = toSceneModel Movie.welcomeMovie,
      keysDown = mempty,
      ..
    }
