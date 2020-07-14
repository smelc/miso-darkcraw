{-# LANGUAGE DataKinds #-}

module Main where

import Board (allCardsSpots)
import Card
import Data.Maybe (mapMaybe)
import Game (attackOrder)
import Json
import Test.Hspec

creatureSum :: [Creature p] -> (Creature p -> Int) -> Int
creatureSum cards getter = sum (map getter cards)

testBalance :: [Card UI] -> Int
testBalance cards =
  undefined
  where
    humanDeck = initialDeck cards Human
    undeadDeck = initialDeck cards Undead

getAllDecks :: [Card UI] -> [[Card Core]]
getAllDecks cards = [initialDeck cards t | t <- allTeams]

isRight (Right _) = True
isRight _ = False

getRight (Right a) = a
getRight _ = error "getRight on Left"

main :: IO ()
main = hspec $ do
  let eitherCards = loadJson
  let cards = getRight eitherCards
  let allDecks = getAllDecks cards
  let allCreatures = mapMaybe cardToCreature $ concat allDecks
  describe "initial state is correct" $ do
    it "cards can be loaded from json" $
      isRight eitherCards -- should be the first test, others depend on it
    it "all decks are initially of the same size" $
      all (\l -> length l == length (head allDecks)) allDecks
    it "all hit points are initially > 0" $
      all (\c -> hp c > 0) allCreatures
    it "all attacks are initially >= 0" $
      all (\c -> attack c >= 0) allCreatures
  describe "attack order contains all spots"
    $ it "check the lengths"
    $ length allCardsSpots `shouldBe` length attackOrder
