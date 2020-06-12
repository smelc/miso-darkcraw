{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

instance ToExpr PlayerPart

instance ToExpr CardSpot

instance ToExpr Board

instance ToExpr HandIndex

instance ToExpr HandFiddle

instance ToExpr Dragging

instance ToExpr Hovering

instance ToExpr Interaction

instance ToExpr Model

-- | Sum type for application events. If drop stuff doesn't work
-- | think whether it's affected by https://github.com/dmjio/miso/issues/478
data Action
  = -- | Dragging card in hand
    DragStart HandIndex
  | DragEnd -- FIXME smelc rename me to Drop
  | DragEnter CardSpot
  | DragLeave CardSpot
  | -- | Starting hovering card in hand
    InHandMouseEnter HandIndex
  | -- | Ending hovering card in hand
    InHandMouseLeave HandIndex
  | NoOp
  | SayHelloWorld
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
  = -- | Playing player puts a card from his hand on its part of the board
    Place HandIndex CardSpot
  | NoPlayAction
  deriving (Show)

noPlayAction :: a -> (a, PlayAction)
noPlayAction interaction = (interaction, NoPlayAction)

-- | Translates an UI event into an 'Interaction' and a 'PlayAction'
updateI :: Action -> Interaction -> (Interaction, PlayAction)
-- This is the only definition that should care about ShowErrorInteraction:
updateI action (ShowErrorInteraction _)
  | action /= NoOp =
    updateI action NoInteraction -- clear error message
    -- Now onto "normal" stuff:
updateI (DragStart i) _ =
  noPlayAction $ DragInteraction $ Dragging i Nothing
updateI DragEnd (DragInteraction Dragging {draggedCard, dragTarget = Just dragTarget}) =
  (NoInteraction, Place draggedCard dragTarget)
updateI DragEnd _ =
  noPlayAction NoInteraction
-- DragEnter cannot create a DragInteraction if there's none yet, we don't
-- want to keep track of drag targets if a drag action did not start yet
updateI (DragEnter cSpot) (DragInteraction dragging) =
  noPlayAction $ DragInteraction $ dragging {dragTarget = Just cSpot}
updateI (DragLeave _) (DragInteraction dragging) =
  noPlayAction $ DragInteraction $ dragging {dragTarget = Nothing}
updateI (InHandMouseEnter i) NoInteraction =
  noPlayAction $ HoverInteraction $ Hovering i
updateI (InHandMouseLeave _) _ =
  noPlayAction NoInteraction
updateI _ i =
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

play :: Model -> PlayAction -> Either Text Model
play m@Model {board} playAction =
  trace ("playing " ++ show playAction) $
  case playAction of
    Place (HandIndex i) cSpot -> do
      -- FIXME smelc can we use boardHand instead ? Yes if the call to
      -- boardToInHandCreaturesToDraw in View.hs preserves the ordering of
      -- the hand (which I believe is the case) meaning the UI index
      -- stored in DragStart and then in Place is valid in terms of Board.
      played :: Creature Core <- lookupInHand uiHand i
      let gameAction :: Game.PlayAction =
            Game.Place (CreatureCard played) cSpot
      board' <- Game.play board gameAction
      return m {board = board'}
      where
        uiHand :: [Creature Core] = boardToInHandCreaturesToDraw board
        boardHand :: [Card Core] = boardToHand board playingPlayerPart
    NoPlayAction ->
      pure m

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel SayHelloWorld m = m <# do consoleLog "miso-darkcraw says hello" >> pure NoOp
updateModel a m@Model {interaction} =
  noEff $ m' {interaction = interaction''}
  where
    (interaction', playAction) = updateI a interaction
    eitherErrModel = Update.play m playAction
    (interaction'', m') =
      case eitherErrModel of
        Left errMsg -> (ShowErrorInteraction errMsg, m)
        Right model' -> (interaction', model')
