{-# LANGUAGE OverloadedStrings #-}

module Update where

import Debug.Trace
import Miso
import Miso.String
import Model

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel action m =
  case action of
    Drag _ -> noEff $ trace "Drag" m
    Drop -> noEff $ trace "Drop" m
    InHandMouseEnter i ->
      noEff $ trace ("entering" ++ show i) $ m { handHover = Just i }
    InHandMouseLeave i ->
      noEff $ trace "exiting" $ m { handHover = Nothing }
    NoOp ->
      noEff m
    SayHelloWorld ->
      m <# do consoleLog "miso-darkcraw says hello" >> pure NoOp
