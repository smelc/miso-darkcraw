{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Update where

import Board
import Card
import Data.TreeDiff
import Debug.Trace
import Miso
import Miso.String
import Model
import Text.PrettyPrint.ANSI.Leijen

-- | Sum type for application events
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

updateI :: Action -> Interaction -> Interaction
updateI (DragStart i) _ =
  DragInteraction $ Dragging i Nothing
updateI DragEnd _ = NoInteraction -- TODO: drop if on drop target
-- DragEnter cannot create a DragInteraction if there's none yet, we don't
-- want to keep track of drag targets if a drag action did not start yet
updateI (DragEnter cSpot) (DragInteraction dragging) =
  DragInteraction $ dragging {dragTarget = Just cSpot}
updateI (DragLeave _) (DragInteraction dragging) =
  DragInteraction $ dragging {dragTarget = Nothing}
updateI (InHandMouseEnter i) NoInteraction = HoverInteraction $ Hovering i
updateI (InHandMouseLeave _) _ = NoInteraction
updateI _ i = i

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel SayHelloWorld m = m <# do consoleLog "miso-darkcraw says hello" >> pure NoOp
updateModel a m@Model {interaction} = noEff $ m {interaction = updateI a interaction}
