{-# LANGUAGE OverloadedStrings #-}

module Update where

import Miso
import Miso.String
import Model

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel action m =
  case action of
    AddOne ->
      noEff (m + 1)
    SubtractOne ->
      noEff (m - 1)
    NoOp ->
      noEff m
    SayHelloWorld ->
      m <# do consoleLog "Hello World" >> pure NoOp
