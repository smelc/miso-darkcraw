{-# LANGUAGE OverloadedStrings #-}

module Update where

import Debug.Trace
import Miso
import Miso.String
import Model

-- | Sum type for application events
data Action
  = DragXY Int Int
  | Drop
  | InHandMouseEnter Int
  | InHandMouseLeave Int
  | NoOp
  | SayHelloWorld
  deriving (Show, Eq)

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel action m =
  case action of
    DragXY _ _ ->
      helper action m
    Drop ->
      helper action m
    InHandMouseEnter i ->
      helper action m {handHover = Just i}
    InHandMouseLeave i ->
      helper action m {handHover = Nothing}
    NoOp ->
      noEff m
    SayHelloWorld ->
      m <# do consoleLog "miso-darkcraw says hello" >> pure NoOp
  where
    helper a m = noEff $ traceShow a m
