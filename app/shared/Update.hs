{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Lens
import Data.Function ((&))
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import Data.TreeDiff
import Debug.Trace
import Game (GamePlayEvent)
import Formatting ((%), format, hex, sformat)
import qualified Game
import Miso
import Miso.String (MisoString, fromMisoString, ms)
import Model
import ServerMessages
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

instance ToExpr GameModel

instance ToExpr PlayingMode

instance ToExpr WelcomeModel

instance ToExpr MultiPlayerLobbyModel

instance ToExpr Model

-- FIXME smelc move Action* to its own file (to avoid cycles later on)

-- | Actions that are raised by 'WelcomeView'
data WelcomeAction
  = WelcomeSelectSinglePlayer Team
  | WelcomeStart
  | WelcomeSelectMultiPlayer
  deriving (Show, Eq)

data MultiPlayerLobbyAction
  = LobbyUpdateUsername MisoString
  | LobbySubmitUsername
  | LobbyServerMessage OutMessage
  deriving (Show, Eq)

-- | Actions that are raised by 'GameView'
data GameAction
  = GameAIPlay
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

data UpdatePlayEvent
  = -- | Player ends its turn
    EndTurn PlayerSpot
  | -- | Playing player puts a card from his hand on its part of the board
    Place PlayerSpot CardSpot HandIndex
  | NoPlayAction
  deriving (Show)

noPlayAction :: a -> (a, UpdatePlayEvent)
noPlayAction interaction = (interaction, NoPlayAction)

-- | Translates a game action into an 'Interaction' and a 'PlayAction'
interpretOnGameModel ::
  GameModel ->
  GameAction ->
  GameInteraction ->
  (GameInteraction, UpdatePlayEvent)
-- This is the only definition that should care about GameShowErrorInteraction:
interpretOnGameModel m action (GameShowErrorInteraction _) =
  interpretOnGameModel m action GameNoInteraction -- clear error message
  -- Now onto "normal" stuff:
interpretOnGameModel m (GameDragStart i) _
  | isPlayerTurn m =
    noPlayAction $ GameDragInteraction $ Dragging i Nothing
interpretOnGameModel
  GameModel {playingPlayer}
  GameDrop
  (GameDragInteraction Dragging {draggedCard, dragTarget = Just dragTarget}) =
    (GameNoInteraction, Place playingPlayer dragTarget draggedCard)
interpretOnGameModel m GameDrop _
  | isPlayerTurn m =
    noPlayAction GameNoInteraction
-- DragEnter cannot create a DragInteraction if there's none yet, we don't
-- want to keep track of drag targets if a drag action did not start yet
interpretOnGameModel m (GameDragEnter cSpot) (GameDragInteraction dragging)
  | isPlayerTurn m =
    noPlayAction $ GameDragInteraction $ dragging {dragTarget = Just cSpot}
interpretOnGameModel m (GameDragLeave _) (GameDragInteraction dragging)
  | isPlayerTurn m =
    noPlayAction $ GameDragInteraction $ dragging {dragTarget = Nothing}
interpretOnGameModel GameModel {board, turn} GameAIPlay _ =
  let actions = aiPlay board turn
   in undefined -- FIXME @smelc create DelayedGameActions from that
interpretOnGameModel m@GameModel {turn} GameEndTurn _
  | isPlayerTurn m =
    (GameNoInteraction, EndTurn $ turnToPlayerSpot turn)
-- Hovering in hand cards
interpretOnGameModel _ (GameInHandMouseEnter i) GameNoInteraction =
  noPlayAction $ GameHoverInteraction $ Hovering i
interpretOnGameModel _ (GameInHandMouseLeave _) _ =
  noPlayAction GameNoInteraction
-- Hovering in place cards
interpretOnGameModel _ (GameInPlaceMouseEnter pSpot cSpot) GameNoInteraction =
  noPlayAction $ GameHoverInPlaceInteraction pSpot cSpot
interpretOnGameModel _ (GameInPlaceMouseLeave _ _) _ =
  noPlayAction GameNoInteraction
-- default
interpretOnGameModel _ _ i =
  noPlayAction i

lookupInHand :: [a] -> Int -> Either Text a
lookupInHand hand i
  | i < 0 = Left $ sformat ("Invalid hand index: " % hex) i
  | i >= handLength =
    Left $
      sformat
        ("Invalid hand index: " % hex % ". Hand has " % hex % " card(s).")
        i
        handLength
  | otherwise = Right (hand !! i)
  where
    handLength = length hand

-- | Event to fire after the given delays. Delays adds up
type DelayedGameActions = [(Int, GameAction)]

microSecsOfSecs :: Num a => a -> a
microSecsOfSecs i = i * 1000000

play ::
  GameModel ->
  UpdatePlayEvent ->
  Either Text (GameModel, DelayedGameActions)
play m@GameModel {board, playingPlayer, turn} playAction = do
  gamePlayAction <- gamify playAction
  case gamePlayAction of
    Nothing -> pure (m, [])
    Just gamePlayAction' -> do
      (board', anims') <- Game.play board gamePlayAction'
      let turn' =
            case gamePlayAction' of
              Game.EndTurn _ -> nextTurn turn
              _ -> turn
      let delayedActions =
            [ (microSecsOfSecs 1, GameAIPlay)
              | turnToPlayerSpot turn' == playingPlayer
            ]
      let m' = m {board = board', turn = turn', anims = anims'}
      return (m', delayedActions)
  where
    gamify :: UpdatePlayEvent -> Either Text (Maybe GamePlayEvent) = \case
      EndTurn pSpot -> return $ Just $ Game.EndTurn pSpot
      Place pSpot cSpot (HandIndex i) -> do
        -- FIXME smelc can we use boardHand instead ? Yes if the call to
        -- boardToInHandCreaturesToDraw in View.hs preserves the ordering of
        -- the hand (which I believe is the case) meaning the UI index
        -- stored in DragStart and then in Place is valid in terms of Board.
        played <- lookupInHand uiHand i
        return $ Just $ Game.Place pSpot cSpot (CreatureCard played)
        where
          pLens = spotToLens pSpot
          uiHand :: [Creature Core] = boardToInHandCreaturesToDraw board pLens
      NoPlayAction -> return Nothing

-- | Updates a 'Gamemodel'
updateGameModel :: GameAction -> GameModel -> GameModel
updateGameModel a m@GameModel {interaction} =
  m' {interaction = interaction''}
  where
    (interaction', playAction) = interpretOnGameModel m a interaction
    eitherErrModel = Update.play m playAction
    (interaction'', m') =
      case eitherErrModel of
        Left errMsg -> (GameShowErrorInteraction errMsg, m)
        Right (model', _) -> (interaction', model') -- FIXME @smelc use data in _

-- | Updates a 'WelcomeModel'
updateWelcomeModel :: WelcomeAction -> WelcomeModel -> WelcomeModel
updateWelcomeModel (WelcomeSelectSinglePlayer team) m =
  m {playingMode = SinglePlayer team}
updateWelcomeModel WelcomeSelectMultiPlayer _ =
  error "WelcomeSelectMultiplayer action should be handled in updateModel"
updateWelcomeModel WelcomeStart _ =
  error "WelcomeStart action should be handled in updateModel"

-- | Updates a 'MultiPlayerLobbyModel'
updateMultiPlayerLobbyModel :: MultiPlayerLobbyAction -> MultiPlayerLobbyModel -> Effect Action MultiPlayerLobbyModel
updateMultiPlayerLobbyModel (LobbyUpdateUsername userName) (CollectingUserName _) =
  noEff $ CollectingUserName (fromMisoString userName)
updateMultiPlayerLobbyModel LobbySubmitUsername (CollectingUserName userName) =
  WaitingForNameSubmission userName <# do
    send (CreateUser userName)
    return NoOp
updateMultiPlayerLobbyModel (LobbyServerMessage UserCreated) (WaitingForNameSubmission userName) =
  noEff $ DisplayingUserList userName []
updateMultiPlayerLobbyModel (LobbyServerMessage (NewUserList users)) (DisplayingUserList userName _) =
  noEff $ DisplayingUserList userName users
updateMultiPlayerLobbyModel a m =
  noEff $ traceShow (a, m) m

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
  m@(WelcomeModel' WelcomeModel {welcomeCards}) =
    noEff $ GameModel' $ initialGameModel welcomeCards
updateModel (WelcomeAction' WelcomeSelectMultiPlayer) (WelcomeModel' _) =
  effectSub (MultiPlayerLobbyModel' (CollectingUserName "")) $
    websocketSub (URL "ws://127.0.0.1:9160") (Protocols []) handleWebSocket
  where
    handleWebSocket (WebSocketMessage action) = MultiPlayerLobbyAction' (LobbyServerMessage action)
    handleWebSocket problem = traceShow problem NoOp
-- Actions that do not change the page, delegate to more specialized versions
updateModel (GameAction' a) (GameModel' m) =
  noEff $ GameModel' $ updateGameModel a m
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

initialGameModel :: [Card UI] -> GameModel
initialGameModel cards =
  GameModel
    board
    GameNoInteraction
    startingPlayerSpot
    initialTurn
    cards
    mempty
  where
    board = exampleBoard cards
