{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Update where

import Board
import Card
import Data.Maybe (fromJust, isJust)
import Data.TreeDiff
import Debug.Trace
import Miso
import Miso.String
import Model
import Text.PrettyPrint.ANSI.Leijen

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
  | DragEnd
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
  = -- | Player puts a card from his on the board
    Place HandIndex PlayerSpot CardSpot
  | NoPlayAction

updateI :: Action -> Interaction -> (Interaction, PlayAction)
-- This is the only definition that should care about ShowErrorInteraction:
updateI action (ShowErrorInteraction _)
  | action /= NoOp =
    updateI action NoInteraction -- clear error message
    -- Now onto "normal" stuff:
updateI (DragStart i) _ =
  (DragInteraction $ Dragging i Nothing, NoPlayAction)
updateI DragEnd (DragInteraction Dragging {draggedCard, dragTarget})
  | isJust dragTarget =
    (NoInteraction, Place draggedCard playingPlayerSpot dragTarget')
  where
    dragTarget' :: CardSpot = fromJust dragTarget
    dragged :: Int = unHandIndex draggedCard
updateI DragEnd _ = (NoInteraction, NoPlayAction)
-- DragEnter cannot create a DragInteraction if there's none yet, we don't
-- want to keep track of drag targets if a drag action did not start yet
updateI (DragEnter cSpot) (DragInteraction dragging) =
  (DragInteraction $ dragging {dragTarget = Just cSpot}, NoPlayAction)
updateI (DragLeave _) (DragInteraction dragging) =
  (DragInteraction $ dragging {dragTarget = Nothing}, NoPlayAction)
updateI (InHandMouseEnter i) NoInteraction =
  (HoverInteraction $ Hovering i, NoPlayAction)
updateI (InHandMouseLeave _) _ = (NoInteraction, NoPlayAction)
updateI _ i = (i, NoPlayAction)

play :: Model -> PlayAction -> Model
play m@Model {board} =
  \case
    Place (HandIndex i) pSpot cSpot -> m -- FIXME smelc forward to board
    NoPlayAction -> m

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel SayHelloWorld m = m <# do consoleLog "miso-darkcraw says hello" >> pure NoOp
updateModel a m@Model {interaction} =
  noEff $ (play m playAction) {interaction = interaction'}
  where
    (interaction', playAction) = updateI a interaction
