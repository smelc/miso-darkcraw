{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Update where

import AI (aiPlay)
import Board
import Card
import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad.Except (runExcept)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Bifunctor as DataBifunctor
import Data.Function ((&))
import Data.Maybe (fromJust, isJust, maybeToList)
import Data.Text (Text)
import Data.TreeDiff
import Debug.Trace
import Formatting ((%), format, hex, sformat)
import Game (GamePlayEvent (..), nextAttackSpot)
import qualified Game
import Miso
import Miso.String (MisoString, fromMisoString, ms)
import Model
import ServerMessages
import System.Random (StdGen)
import Text.PrettyPrint.ANSI.Leijen
import Text.Printf
import Turn (Turn, initialTurn, nextTurn, turnToPlayerSpot)

instance ToExpr CreatureKind

instance ToExpr Team

instance ToExpr CreatureID

instance ToExpr Skill

instance ToExpr Neutral

instance ToExpr Item

instance ToExpr (Creature Core)

instance ToExpr (Creature UI)

instance ToExpr (Card Core)

instance ToExpr (Card UI)

instance ToExpr PlayerSpot

instance ToExpr AttackEffect

instance ToExpr AttackEffects

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

instance ToExpr SharedModel

instance ToExpr GameModel

instance ToExpr PlayingMode

instance ToExpr SinglePlayerLobbyModel

instance ToExpr WelcomeModel

instance ToExpr MultiPlayerLobbyModel

instance ToExpr MultiPlayerLobbyError

instance ToExpr InvitationState

instance ToExpr InvitedState

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

-- | Sum type for application events. If drop stuff doesn't work
-- | think whether it's affected by https://github.com/dmjio/miso/issues/478
data Action
  = GameAction' GameAction
  | NoOp
  | MultiPlayerLobbyAction' MultiPlayerLobbyAction
  | SayHelloWorld
  | SinglePlayerLobbyAction' SinglePlayerLobbyAction
  | -- Leave 'SinglePlayerView', back to 'WelcomeView'
    SinglePlayerBack
  | -- Leave 'SinglePlayerView', start game
    SinglePlayerGo
  | -- Leave 'WelcomeView', go to 'MultiPlayerView' or 'SinglePlayerView'
    WelcomeGo WelcomeDestination
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
  where
    turn' = nextTurn turn
    m' = m {turn = turn'}
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
  (InvitingUser me users user WaitingForRSVP) =
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
delayActions :: Model -> [(Int, Action)] -> Effect Action Model
delayActions m actions =
  Effect
    m
    [ \sink -> do
        liftIO $ threadDelay i
        liftIO (sink action)
      | (i, action) <- actions
    ]

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
  m <# do consoleLog "miso-darkcraw says hello" >> pure NoOp
-- Actions that change the page
-- Actions that leave 'SinglePlayerView'
updateModel
  SinglePlayerBack
  m@(SinglePlayerLobbyModel' SinglePlayerLobbyModel {singlePlayerLobbyShared = shared}) =
    noEff $ WelcomeModel' $ WelcomeModel shared
updateModel
  SinglePlayerGo
  m@( SinglePlayerLobbyModel'
        SinglePlayerLobbyModel
          { singlePlayerLobbyTeam = Just team,
            singlePlayerLobbyShared = shared
          }
      ) =
    noEff $ GameModel' $ initialGameModel shared
-- Actions that leave 'WelcomeView'
updateModel
  (WelcomeGo SinglePlayerDestination)
  m@(WelcomeModel' WelcomeModel {welcomeShared}) =
    noEff $ SinglePlayerLobbyModel' $ SinglePlayerLobbyModel Nothing welcomeShared
updateModel (WelcomeGo MultiPlayerDestination) (WelcomeModel' _) =
  effectSub (MultiPlayerLobbyModel' (CollectingUserName "")) $
    websocketSub (URL "ws://127.0.0.1:9160") (Protocols []) handleWebSocket
  where
    handleWebSocket (WebSocketMessage action) = MultiPlayerLobbyAction' (LobbyServerMessage action)
    handleWebSocket problem = traceShow problem NoOp
-- Actions that do not change the page, delegate to more specialized versions
updateModel (GameAction' a) (GameModel' m@GameModel {interaction}) =
  if null actions
    then noEff m''
    else delayActions m'' $ prepare $ check actions
  where
    (m', actions) = updateGameModel m a interaction
    m'' = GameModel' m'
    sumDelays _ [] = []
    sumDelays d ((i, a) : tl) = (d + i, a) : sumDelays (d + i) tl
    prepare as = map (\(i, a) -> (i * toSecs, GameAction' a)) $ sumDelays 0 as
    toSecs = 1000000
    check ((0, event) : _) = error $ "updateGameModel should not return event with 0 delay, but " ++ show event ++ " did"
    check as = as
updateModel (SinglePlayerLobbyAction' a) (SinglePlayerLobbyModel' m) =
  noEff $ SinglePlayerLobbyModel' $ updateSinglePlayerLobbyModel a m
updateModel (MultiPlayerLobbyAction' a) (MultiPlayerLobbyModel' m) =
  MultiPlayerLobbyModel' `fmap` updateMultiPlayerLobbyModel a m
updateModel a m =
  error $
    "Unhandled case in updateModel with the model being:\n"
      ++ show m
      ++ "\nand the action being:\n"
      ++ show a

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
