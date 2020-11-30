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

import AI
import Board
import Card
import Cinema (Actor, ActorState, Direction, Element, Frame, Scene, TimedFrame (TimedFrame, duration), render)
import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import qualified Data.Bifunctor as DataBifunctor
import Data.Foldable (asum, toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.Lazy as Text
import Data.TreeDiff
import qualified Data.Vector as V
import Debug.Trace
import qualified Game (Event (..), Result (..), Target (..), drawCards, nbCardsToDraw, nextAttackSpot, play, playAll, transferCards)
import Miso
import Miso.String (MisoString, fromMisoString)
import Model
import Movie (welcomeMovie)
import ServerMessages
import SharedModel (SharedModel (..))
import System.Random (StdGen)
import Text.Pretty.Simple
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Tile
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

instance ToExpr (NeutralObject Core)

instance ToExpr (NeutralObject UI)

instance ToExpr (Card Core)

instance ToExpr (Card UI)

instance ToExpr PlayerSpot

instance ToExpr InPlaceEffect

instance ToExpr InPlaceEffects

instance ToExpr CardIdentifier

instance ToExpr (PlayerPart Core)

instance ToExpr (PlayerPart UI)

instance ToExpr CardSpot

instance ToExpr (Board Core)

instance ToExpr (Board UI)

instance ToExpr HandIndex

instance ToExpr HandFiddle

instance ToExpr Game.Target

instance ToExpr Dragging

instance ToExpr Hovering

instance ToExpr GameInteraction

instance ToExpr Turn

instance ToExpr StdGen where toExpr = defaultExprViaShow

instance ToExpr Tile.TileUI

instance ToExpr Card.SkillUI

instance ToExpr SharedModel

instance ToExpr GameModel

instance ToExpr PlayingMode

instance ToExpr SinglePlayerLobbyModel

instance ToExpr Cinema.Direction

instance ToExpr Cinema.ActorState

instance ToExpr Tile.Tile

instance ToExpr Cinema.Element

instance ToExpr TimedFrame

instance ToExpr Cinema.Actor

instance ToExpr Cinema.Frame

instance ToExpr SceneModel

instance ToExpr WelcomeModel

instance ToExpr MultiPlayerLobbyModel

instance ToExpr MultiPlayerLobbyError

instance ToExpr InvitationActorState

instance ToExpr InvitedActorState

instance ToExpr DeckModel

instance ToExpr Model

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
    GamePlay Game.Event
  | -- | Turn was updated previously by 'GameIncrTurn',
    -- | time to draw cards from the stack. Then the handler of this event
    -- | will take care of giving the player control back. The Int parameter
    -- | indicates how many cards can be drawn. Stream of events terminates
    -- | when it becomes 0 or when hand is full.
    GameDrawCard Int
  | -- | All actions have been resolved, time to update the turn widget
    -- | and to schedule 'GameDrawCard'. This does NOT translate
    -- | to a 'GamePlayEvent'.
    GameIncrTurn
  | -- | Dragging card in hand ends. When a successful drop is done,
    -- this event is fired right after GameDrop. We rely on that. If GameDrop
    -- was fired last, we would miss it. Be careful on untested browsers.
    GameDragEnd
  | -- | Dragging card in hand
    GameDragStart HandIndex
  | -- | This can play the 'GamePlayEvent' 'Place'
    GameDrop
  | GameDragEnter Game.Target
  | GameDragLeave Game.Target
  | -- | End Turn button pressed in turn widget. For player, schedule
    -- | attacks then 'GameIncrTurn'; for AI, compute its actions,
    -- | schedule them, and then schedule attack and 'GameIncrTurn'.
    GameEndTurnPressed
  | -- | Starting hovering card in hand
    GameInHandMouseEnter HandIndex
  | -- | Ending hovering card in hand
    GameInHandMouseLeave HandIndex
  | -- | Starting hovering a target
    GameInPlaceMouseEnter Game.Target
  | -- | Ending hovering a target
    GameInPlaceMouseLeave Game.Target
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

playOne :: GameModel -> Game.Event -> (GameModel, GameActionSeq)
playOne m gamePlayEvent =
  updateGameModel m (GamePlay gamePlayEvent) GameNoInteraction

-- | We MUST return GameAction as the second element (as opposed to
-- 'GamePlayEvent'). This is used for 'GameIncrTurn' for example.
-- The second element must only contain delayed stuff, it's invalid
-- to return a list whose first element has 0. If we did that, we would
-- need to play this event right away, and then we would have new delayed
-- actions to consider which we wouldn't know how to merge with the first ones.
-- If you feel like adding a 0 event, you instead need to play this event
-- right away by doing a recursive call (or use 'playOne')
updateGameModel ::
  GameModel ->
  GameAction ->
  GameInteraction ->
  (GameModel, GameActionSeq)
-- This is the only definition that should care about GameShowErrorInteraction:
updateGameModel m action (GameShowErrorInteraction _) =
  updateGameModel m action GameNoInteraction -- clear error message
  -- Now onto "normal" stuff:
updateGameModel m GameDragEnd _ = withInteraction m GameNoInteraction
updateGameModel m (GameDragStart i) _
  | isPlayerTurn m =
    withInteraction m $ GameDragInteraction $ Dragging i Nothing
updateGameModel
  m
  GameDrop
  (GameDragInteraction Dragging {draggedCard, dragTarget = Just dragTarget}) =
    playOne m $ Game.Place dragTarget draggedCard
updateGameModel m GameDrop _
  | isPlayerTurn m =
    withInteraction m GameNoInteraction
-- DragEnter cannot create a DragInteraction if there's none yet, we don't
-- want to keep track of drag targets if a drag action did not start yet
updateGameModel m (GameDragEnter target) (GameDragInteraction dragging)
  | isPlayerTurn m =
    withInteraction m $ GameDragInteraction $ dragging {dragTarget = Just target}
updateGameModel m (GameDragLeave _) (GameDragInteraction dragging)
  | isPlayerTurn m =
    withInteraction m $ GameDragInteraction $ dragging {dragTarget = Nothing}
-- A GamePlayEvent to execute
updateGameModel m@GameModel {board, gameShared} (GamePlay gameEvent) _ =
  case Game.play gameShared board gameEvent of
    Left errMsg -> withInteraction m $ GameShowErrorInteraction errMsg
    Right (Game.Result board' anims') ->
      -- There MUST be a delay here, otherwise it means we would need
      -- to execute this event now. We don't want that. 'playAll' checks that.
      (m', zip (repeat 1) $ maybeToList event)
      where
        m' = m {board = board', anims = anims'}
        event = case gameEvent of
          Game.Attack pSpot cSpot continue end ->
            -- enqueue resolving next attack if applicable
            case (continue, Game.nextAttackSpot board pSpot (Just cSpot)) of
              (False, _) -> terminator
              (True, Nothing) -> terminator
              (True, Just cSpot') -> Just $ GamePlay $ Game.Attack pSpot cSpot' True end
            where
              terminator = if end then Just GameIncrTurn else Nothing
          _ -> Nothing
updateGameModel m@GameModel {board, gameShared, turn} (GameDrawCard n) _ =
  case Game.drawCards gameShared board pSpot 1 of
    Left errMsg -> withInteraction m $ GameShowErrorInteraction errMsg
    Right (board', boardui', shared') ->
      ( m {board = board', anims = boardui', gameShared = shared'},
        -- enqueue next event (if any)
        [(1, GameDrawCard $ n - 1) | n - 1 >= 1]
      )
  where
    pSpot = assert (n >= 1) $ turnToPlayerSpot turn
-- "End Turn" button pressed by the player or the AI
updateGameModel m@GameModel {board, gameShared, turn} GameEndTurnPressed _ =
  case em' of
    Left err -> withInteraction m $ GameShowErrorInteraction err
    Right m'@GameModel {board = board'} ->
      if isInitialTurn
        then -- We want a one second delay, to see clearly that the opponent
        -- put its cards, and then proceed with resolving attacks
          (m', [(1, event)])
        else -- We don't want any delay so that the game feels responsive
        -- when the player presses "End Turn", hence the recursive call.
          updateGameModel m' event GameNoInteraction
      where
        event =
          -- schedule resolving first attack
          case Game.nextAttackSpot board' pSpot Nothing of
            Nothing -> GameIncrTurn -- no more attack, change turn
            Just cSpot -> GamePlay $ Game.Attack pSpot cSpot True True
  where
    pSpot = turnToPlayerSpot turn
    isInitialTurn = turn == initialTurn
    em' =
      if isInitialTurn
        then do
          -- End Turn pressed at the end of the player's first turn, make the AI
          -- place its card in a state where the player did not put its
          -- card yet, then place them all at once; and then continue
          -- Do not reveal player placement to AI
          let emptyPlayerInPlaceBoard = boardSetInPlace board pSpot Map.empty
          let placements = AI.placeCards gameShared emptyPlayerInPlaceBoard $ nextTurn turn
          Game.Result board' boardui' <- Game.playAll gameShared board placements
          return $ m {anims = boardui', board = board'}
        else Right m
updateGameModel m@GameModel {gameShared, playingPlayer, turn} GameIncrTurn _ =
  (m'', events)
  where
    turn' = nextTurn turn
    m' = m {turn = turn'}
    pSpot = turnToPlayerSpot turn'
    isAI = pSpot /= playingPlayer
    nbDraws = Game.nbCardsToDraw (Model.board m') pSpot
    -- If it's the player turn, we wanna handle draw the first card right away,
    -- so that the game feels responsive.
    nbDraws' = if isAI then nbDraws else min 1 nbDraws
    (m'', events) =
      case runEither of
        Left errMsg -> withInteraction m' $ GameShowErrorInteraction errMsg
        Right (events, board', boardui', shared') ->
          ( m' {anims = boardui', board = board', gameShared = shared'},
            if isAI
              then -- AI: after drawing cards and playing its events
              -- press "End Turn". We want a one second delay, it makes
              -- it easier to understand what's going on
                zip (repeat 1) $ snoc (map GamePlay events) GameEndTurnPressed
              else -- player: we drew the first card already (see nbDraws')
              -- enqueue next event (if any)
                assert (null events) [(1, GameDrawCard $ nbDraws - 1) | nbDraws -1 >= 1]
          )
      where
        -- both: transfer cards from discarded stack to stack, if necessary
        -- AI: draw all cards, compute its events
        -- player: draw first card
        runEither = do
          let (shared', board', boardui') =
                Game.transferCards (Model.gameShared m') (Model.board m') pSpot
          (board'', boardui'', shared'') <-
            Game.drawCards shared' board' pSpot nbDraws'
          let aiEvents = if isAI then aiPlay gameShared board'' turn' else []
          return (aiEvents, board'', boardui' <> boardui'', shared'')
-- Hovering in hand cards
updateGameModel m (GameInHandMouseEnter i) GameNoInteraction =
  withInteraction m $ GameHoverInteraction $ Hovering i
updateGameModel m (GameInHandMouseLeave _) _ =
  withInteraction m GameNoInteraction
-- Hovering in place cards
updateGameModel m (GameInPlaceMouseEnter target) GameNoInteraction =
  withInteraction m $ GameHoverInPlaceInteraction target
updateGameModel m (GameInPlaceMouseLeave _) _ =
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
        { singlePlayerLobbyTeam = Just team,
          singlePlayerLobbyShared = shared
        }
    ) =
    noEff $ GameModel' $ initialGameModel shared team
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
  error $
    Text.unpack $
      "Unhandled case in updateModel with the model being:\n"
        <> pShowNoColor m
        <> "\nand the action being:\n"
        <> pShowNoColor a

initialGameModel ::
  SharedModel ->
  -- | The team of the playing player
  Team ->
  GameModel
initialGameModel shared team =
  GameModel
    shared'
    board
    GameNoInteraction
    startingPlayerSpot
    initialTurn
    mempty
  where
    -- We hardcode the position of the playing player here. No big deal.
    teams = Teams {topTeam = Undead, botTeam = team}
    (shared', board) = initialBoard shared teams

initialWelcomeModel :: SharedModel -> WelcomeModel
initialWelcomeModel welcomeShared =
  WelcomeModel
    { welcomeSceneModel = toSceneModel Movie.welcomeMovie,
      keysDown = mempty,
      ..
    }
