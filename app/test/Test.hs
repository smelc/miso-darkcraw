{-# LANGUAGE DataKinds #-}

module Main where

import Board (allCardsSpots)
import Card
import Game (attackOrder)
import Json
import Test.Hspec

creatureSum :: [Creature p] -> (Creature p -> Int) -> Int
creatureSum cards getter =
  foldr (\c i -> getter c + i) 0 cards

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
  describe "initial state is correct" $ do
    it "cards can be loaded from json" $
      isRight eitherCards -- should be the first test, others depend on it
    xit "all decks are initially of the same size" $ -- TODO unxit me
      -- when Undead deck is implemented
      all (\l -> length l == length (head allDecks)) allDecks
  describe "attack order contains all spots"
    $ it "check the lengths"
    $ length allCardsSpots `shouldBe` length attackOrder
