{-# LANGUAGE OverloadedStrings #-}

module Update where

import Miso
import Miso.String
import Model

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel action m =
  case action of
    InHandMouseEnter i ->
      noEff $ m { handHover = Just i }
    InHandMouseLeave i ->
      noEff m { handHover = Nothing }
    NoOp ->
      noEff m
    SayHelloWorld ->
      m <# do consoleLog "miso-darkcraw says hello" >> pure NoOp
