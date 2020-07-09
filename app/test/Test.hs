{-# LANGUAGE DataKinds #-}

module Main where

import Board (allCardsSpots)
import Card
import Game (attackOrder)
import Test.Hspec

creatureSum :: [Creature p] -> (Creature p -> Int) -> Int
creatureSum cards getter =
  foldr (\c i -> getter c + i) 0 cards

testBalance :: [Card UI] -> Int
testBalance cards =
  undefined
  where
    humanDeck = defaultDeck cards Human
    undeadDeck = defaultDeck cards Undead

main :: IO ()
main = hspec $ do
  describe "attack order contains all spots" $ do
    it "check the lengths" $ do
      length allCardsSpots `shouldBe` length attackOrder