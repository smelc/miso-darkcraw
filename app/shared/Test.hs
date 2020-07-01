module Main where

import Board (allCardsSpots)
import Game (attackOrder)
import System.Exit (exitFailure, exitSuccess, exitWith)

testAttackOrder :: Int
testAttackOrder =
  if length allCardsSpots == length attackOrder
    then 0
    else 1

-- TODO use a test framework
main :: IO Int
main =
  let rc = testAttackOrder
   in case rc of
        0 -> exitSuccess
        _ -> exitFailure
