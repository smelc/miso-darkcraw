{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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
  | Drop
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

instance ToExpr Model

logUpdates :: (Show a, Eq m, ToExpr m) => (a -> m -> Effect a m) -> a -> m -> Effect a m
logUpdates update action model = do
  model' <- update action model
  return $
    trace
      (show action ++ "\n" ++ diff model model')
      model'
  where
    diff model model'
      | model == model' = "no diff"
      | otherwise = prettyDiff (ediff model model')
    prettyDiff edits = displayS (renderPretty 0.4 80 (ansiWlEditExprCompact edits)) ""

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel (DragStart i) m = noEff $ m {handFiddle = Just $ HandDragging i}
updateModel DragEnd m@Model{handFiddle=Just _} = noEff $ m {handFiddle = Nothing}
updateModel Drop m@Model{handFiddle=Just _} = noEff $ m {handFiddle = Nothing}
updateModel (InHandMouseEnter i) m@Model{handFiddle=Nothing} = noEff $ m {handFiddle = Just $ HandHovering i}
updateModel (InHandMouseLeave i) m@Model{handFiddle=Just (HandHovering _)} = noEff $ m {handFiddle = Nothing}
updateModel SayHelloWorld m = m <# do consoleLog "miso-darkcraw says hello" >> pure NoOp
updateModel _ m = noEff m
