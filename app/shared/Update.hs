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
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import Data.TreeDiff
import Debug.Trace
import Formatting ((%), format, hex, sformat)
import Game (GamePlayEvent (..))
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

instance ToExpr WelcomeModel

instance ToExpr MultiPlayerLobbyModel

instance ToExpr MultiPlayerLobbyError

instance ToExpr InvitationState

instance ToExpr InvitedState

instance ToExpr Model

-- FIXME smelc move Action* to its own file (to avoid cycles later on)

-- | Actions that are raised by 'WelcomeView'
data WelcomeAction
  = -- | Click on "Single Player"
    WelcomeSelectSinglePlayer
  | -- | Select a team in Single Player mode
    WelcomeSelectSinglePlayerTeam Team
  | WelcomeStart
  | -- | Click on "Multiplayer"
    WelcomeSelectMultiPlayer
  deriving (Show, Eq)

data MultiPlayerLobbyAction
  = LobbyUpdateUsername MisoString
  | LobbySubmitUsername
  | LobbyInviteUser UserName
  | CancelInvitationClicked
  | AcceptInvitationClicked
  | RejectInvitationClicked
  | LobbyServerMessage OutMessage
  deriving (Show, Eq)

-- | Actions that are raised by 'GameView'
data GameAction
  = GameAIPlay GamePlayEvent
  | -- | Dragging card in hand
    GameDragStart HandIndex
  | GameDrop
  | GameDragEnter CardSpot
  | GameDragLeave CardSpot
  | -- | End Turn button pressed in turn widget
    GameEndTurn
  | -- | Starting hovering card in hand
    GameInHandMouseEnter HandIndex
  | -- | Ending hovering card in hand
    GameInHandMouseLeave HandIndex
  | -- | Starting hovering card in place
    GameInPlaceMouseEnter PlayerSpot CardSpot
  | -- | Ending hovering card in place
    GameInPlaceMouseLeave PlayerSpot CardSpot
  deriving (Show, Eq)

-- | Sum type for application events. If drop stuff doesn't work
-- | think whether it's affected by https://github.com/dmjio/miso/issues/478
data Action
  = GameAction' GameAction
  | NoOp
  | SayHelloWorld
  | WelcomeAction' WelcomeAction
  | MultiPlayerLobbyAction' MultiPlayerLobbyAction
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

noPlayEvent :: a -> (a, GamePlayEvent)
noPlayEvent interaction = (interaction, NoPlayEvent)

noGameInteraction :: GamePlayEvent -> (GameInteraction, GamePlayEvent)
noGameInteraction gameEvent = (GameNoInteraction, gameEvent)

interpretOnGameModel ::
  GameModel ->
  GameAction ->
  GameInteraction ->
  (GameInteraction, GamePlayEvent)
-- This is the only definition that should care about GameShowErrorInteraction:
interpretOnGameModel m action (GameShowErrorInteraction _) =
  interpretOnGameModel m action GameNoInteraction -- clear error message
  -- Now onto "normal" stuff:
interpretOnGameModel m (GameDragStart i) _
  | isPlayerTurn m =
    noPlayEvent $ GameDragInteraction $ Dragging i Nothing
interpretOnGameModel
  GameModel {playingPlayer}
  GameDrop
  (GameDragInteraction Dragging {draggedCard, dragTarget = Just dragTarget}) =
    noGameInteraction $ Place playingPlayer dragTarget draggedCard
interpretOnGameModel m GameDrop _
  | isPlayerTurn m =
    noPlayEvent GameNoInteraction
-- DragEnter cannot create a DragInteraction if there's none yet, we don't
-- want to keep track of drag targets if a drag action did not start yet
interpretOnGameModel m (GameDragEnter cSpot) (GameDragInteraction dragging)
  | isPlayerTurn m =
    noPlayEvent $ GameDragInteraction $ dragging {dragTarget = Just cSpot}
interpretOnGameModel m (GameDragLeave _) (GameDragInteraction dragging)
  | isPlayerTurn m =
    noPlayEvent $ GameDragInteraction $ dragging {dragTarget = Nothing}
interpretOnGameModel m@GameModel {board, turn} (GameAIPlay gameEvent) _
  | not $ isPlayerTurn m =
    noGameInteraction gameEvent
interpretOnGameModel m@GameModel {turn} GameEndTurn _
  | isPlayerTurn m =
    noGameInteraction $ EndTurn $ turnToPlayerSpot turn
-- Hovering in hand cards
interpretOnGameModel _ (GameInHandMouseEnter i) GameNoInteraction =
  noPlayEvent $ GameHoverInteraction $ Hovering i
interpretOnGameModel _ (GameInHandMouseLeave _) _ =
  noPlayEvent GameNoInteraction
-- Hovering in place cards
interpretOnGameModel _ (GameInPlaceMouseEnter pSpot cSpot) GameNoInteraction =
  noPlayEvent $ GameHoverInPlaceInteraction pSpot cSpot
interpretOnGameModel _ (GameInPlaceMouseLeave _ _) _ =
  noPlayEvent GameNoInteraction
-- default
interpretOnGameModel _ _ i =
  noPlayEvent i

-- | Event to fire after the given delays (in seconds). Delays add up.
type GameActionSeq = [(Int, GameAction)]

play ::
  GameModel ->
  GamePlayEvent ->
  Either Text (GameModel, GameActionSeq)
play m@GameModel {board, playingPlayer, turn} playAction = do
  (board', anims') <- Game.play board playAction
  let turn' =
        case playAction of
          Game.EndTurn _ -> nextTurn turn
          _ -> turn
  delayedActions <-
    if turnToPlayerSpot turn' == playingPlayer
      then pure []
      else do
        aiEvents <- aiPlay board' turn'
        pure $ map GameAIPlay aiEvents & zip [1 ..] -- 1 second between each event
  let m' = m {board = board', turn = turn', anims = anims'}
  return (m', delayedActions)

-- | Updates a 'Gamemodel'
updateGameModel :: GameAction -> GameModel -> (GameModel, GameActionSeq)
updateGameModel a m@GameModel {interaction} =
  case play m playEvent of
    Left errMsg -> (m {interaction = GameShowErrorInteraction errMsg}, [])
    Right (m', as) -> (m' {interaction = interaction'}, as)
  where
    (interaction', playEvent) = interpretOnGameModel m a interaction

-- | Updates a 'WelcomeModel'
updateWelcomeModel :: WelcomeAction -> WelcomeModel -> WelcomeModel
updateWelcomeModel WelcomeSelectSinglePlayer m =
  m {playingMode = SinglePlayer}
updateWelcomeModel (WelcomeSelectSinglePlayerTeam team) m =
  m {playingMode = SinglePlayerTeam team}
updateWelcomeModel WelcomeSelectMultiPlayer _ =
  error "WelcomeSelectMultiplayer action should be handled in updateModel"
updateWelcomeModel WelcomeStart _ =
  error "WelcomeStart action should be handled in updateModel"

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
updateModel
  (WelcomeAction' WelcomeStart)
  m@(WelcomeModel' WelcomeModel {welcomeShared}) =
    noEff $ GameModel' $ initialGameModel welcomeShared
updateModel (WelcomeAction' WelcomeSelectMultiPlayer) (WelcomeModel' _) =
  effectSub (MultiPlayerLobbyModel' (CollectingUserName "")) $
    websocketSub (URL "ws://127.0.0.1:9160") (Protocols []) handleWebSocket
  where
    handleWebSocket (WebSocketMessage action) = MultiPlayerLobbyAction' (LobbyServerMessage action)
    handleWebSocket problem = traceShow problem NoOp
-- Actions that do not change the page, delegate to more specialized versions
updateModel (GameAction' a) (GameModel' m) =
  if null actions
    then noEff m''
    else delayActions m'' actions'
  where
    (m', actions) = updateGameModel a m
    m'' = GameModel' m'
    sumDelays _ [] = []
    sumDelays d ((i, a) : tl) = (d + i, a) : sumDelays (d + i) tl
    actions' = map (\(i, a) -> (i * toSecs, GameAction' a)) $ sumDelays 0 actions
    toSecs = 1000000
updateModel (WelcomeAction' a) (WelcomeModel' m) =
  noEff $ WelcomeModel' $ updateWelcomeModel a m
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
