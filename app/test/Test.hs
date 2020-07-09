{-# LANGUAGE DataKinds #-}

module Main where

import Board (allCardsSpots)
import Card
import Game (attackOrder)
import System.Exit (exitFailure, exitSuccess, exitWith)

testAttackOrder :: Int
testAttackOrder =
  if length allCardsSpots == length attackOrder
    then 0
    else 1

creatureSum :: [Creature p] -> (Creature p -> Int) -> Int
creatureSum cards getter =
  foldr (\c i -> getter c + i) 0 cards

testBalance :: [Card UI] -> Int
testBalance cards =
  undefined
  where
    humanDeck = defaultDeck cards Human
    undeadDeck = defaultDeck cards Undead

-- TODO use a test framework
-- TODO Loads the json data to obtain a [Card UI] value
main :: IO Int
main =
  let rc = testAttackOrder
   in case rc of
        0 -> exitSuccess
        _ -> exitFailure
