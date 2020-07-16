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

import Board
import Card
import Control.Lens
import Data.Function ((&))
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import Data.TreeDiff
import Debug.Trace
import Formatting ((%), format, hex, sformat)
import qualified Game
import Miso
import Miso.String (ms)
import Model
import Text.PrettyPrint.ANSI.Leijen
import Text.Printf
import Turn (Turn, initialTurn)

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

instance ToExpr Interaction

instance ToExpr Turn

instance ToExpr GameModel

instance ToExpr WelcomeModel

instance ToExpr Model

data WelcomeAction =
  WelcomeStart
  deriving (Show, Eq)

-- | Sum type for application events. If drop stuff doesn't work
-- | think whether it's affected by https://github.com/dmjio/miso/issues/478
data Action
  = -- | Dragging card in hand
    DragStart HandIndex
  | DragEnd -- FIXME smelc rename me to Drop
  | DragEnter CardSpot
  | DragLeave CardSpot
  | -- | End Turn button pressed in turn widget
    EndTurn
  | -- | Starting hovering card in hand
    InHandMouseEnter HandIndex
  | -- | Ending hovering card in hand
    InHandMouseLeave HandIndex
  | -- | Starting hovering card in place
    InPlaceMouseEnter PlayerSpot CardSpot
  | -- | Ending hovering card in place
    InPlaceMouseLeave PlayerSpot CardSpot
  | NoOp
  | SayHelloWorld
  | WelcomeAction' WelcomeAction
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

data PlayAction
  = -- | Player ends its turn
    EndPlayerTurn PlayerSpot
  | -- | Playing player puts a card from his hand on its part of the board
    Place PlayerSpot CardSpot HandIndex
  | NoPlayAction
  deriving (Show)

noPlayAction :: a -> (a, PlayAction)
noPlayAction interaction = (interaction, NoPlayAction)

-- | Translates an UI event into an 'Interaction' and a 'PlayAction'
updateI :: Model -> Action -> Interaction -> (Interaction, PlayAction)
-- This is the only definition that should care about ShowErrorInteraction:
updateI m action (ShowErrorInteraction _)
  | action /= NoOp =
    updateI m action NoInteraction -- clear error message
    -- Now onto "normal" stuff:
updateI _ (DragStart i) _ =
  noPlayAction $ DragInteraction $ Dragging i Nothing
updateI (GameModel' GameModel {playingPlayer}) DragEnd (DragInteraction Dragging {draggedCard, dragTarget = Just dragTarget}) =
  (NoInteraction, Place playingPlayer dragTarget draggedCard)
updateI _ DragEnd _ =
  noPlayAction NoInteraction
-- DragEnter cannot create a DragInteraction if there's none yet, we don't
-- want to keep track of drag targets if a drag action did not start yet
updateI _ (DragEnter cSpot) (DragInteraction dragging) =
  noPlayAction $ DragInteraction $ dragging {dragTarget = Just cSpot}
updateI _ (DragLeave _) (DragInteraction dragging) =
  noPlayAction $ DragInteraction $ dragging {dragTarget = Nothing}
updateI (GameModel' GameModel {playingPlayer}) EndTurn _ =
  (NoInteraction, EndPlayerTurn playingPlayer)
-- Hovering in hand cards
updateI _ (InHandMouseEnter i) NoInteraction =
  noPlayAction $ HoverInteraction $ Hovering i
updateI _ (InHandMouseLeave _) _ =
  noPlayAction NoInteraction
-- Hovering in place cards
updateI _ (InPlaceMouseEnter pSpot cSpot) NoInteraction =
  noPlayAction $ HoverInPlaceInteraction pSpot cSpot
updateI _ (InPlaceMouseLeave _ _) _ =
  noPlayAction NoInteraction
-- default
updateI _ _ i =
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

play :: GameModel -> PlayAction -> Either Text GameModel
play m@GameModel {board} playAction =
  trace ("playing " ++ show playAction) $ do
    gamePlayAction <- gamify playAction
    case gamePlayAction of
      Nothing -> pure m
      Just gamePlayAction' -> do
        (board', anims') <- Game.play board gamePlayAction'
        return $ m {board = board', anims = anims'}
  where
    gamify :: PlayAction -> Either Text (Maybe Game.PlayAction) = \case
      EndPlayerTurn pSpot -> return $ Just $ Game.EndPlayerTurn pSpot
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
          boardHand :: [Card Core] = boardToHand board pLens
      NoPlayAction -> return Nothing

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel SayHelloWorld m = m <# do consoleLog "miso-darkcraw says hello" >> pure NoOp
updateModel _ m@(WelcomeModel' _) = noEff m
updateModel a gm@(GameModel' m@GameModel {interaction}) =
  noEff $ GameModel' $ m' {interaction = interaction''}
  where
    (interaction', playAction) = updateI gm a interaction
    eitherErrModel = Update.play m playAction
    (interaction'', m') =
      case eitherErrModel of
        Left errMsg -> (ShowErrorInteraction errMsg, m)
        Right model' -> (interaction', model')

initialGameModel :: [Card UI] -> GameModel
initialGameModel cards =
  GameModel
    board
    NoInteraction
    startingPlayerSpot
    initialTurn
    cards
    mempty
  where
    board = exampleBoard cards
