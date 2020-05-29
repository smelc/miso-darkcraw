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
-- | It'd be better to have HandIndex the type of the first argument of
-- | DragXY, InHandMouseenter and InHandMouseLeave but it's inconvenient :-()
-- | XXX Should I use a type alias instead?
data Action
  = -- | Dragging card in hand
    DragXY Int Int Int
  | Drop
  | -- | Starting hovering card in hand
    InHandMouseEnter Int
  | -- | Ending hovering card in hand
    InHandMouseLeave Int
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
updateModel action m =
  case action of
    DragXY {} ->
      noEff m
    Drop ->
      noEff m
    InHandMouseEnter i ->
      noEff $ m {handHover = Just $ HandIndex i}
    InHandMouseLeave i ->
      noEff $ m {handHover = Nothing}
    NoOp ->
      noEff m
    SayHelloWorld ->
      m <# do consoleLog "miso-darkcraw says hello" >> pure NoOp
