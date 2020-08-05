{-# LANGUAGE DataKinds #-}

module Main where

import AI (aiPlay)
import Board (Board, allCardsSpots, exampleBoard)
import Card
import Constants
import Control.Lens
import Control.Lens.Extras
import Control.Monad.Except (runExcept)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Game (GamePlayEvent, attackOrder)
import Json
import Test.Hspec
import Turn (Turn, initialTurn, nextTurn)

creatureSum :: [Creature p] -> (Creature p -> Int) -> Int
creatureSum cards getter = sum (map getter cards)

getAllDecks :: [Card UI] -> [[Card Core]]
getAllDecks cards = [initialDeck cards t | t <- allTeams]

testBalance :: [Card UI] -> Int
testBalance cards =
  undefined
  where
    humanDeck = initialDeck cards Human
    undeadDeck = initialDeck cards Undead

testAI :: Board Core -> Turn -> Either Text [GamePlayEvent]
testAI board turn =
  aiPlay board turn & runExcept

main :: IO ()
main = hspec $ do
  let eitherCards = loadJson
  let cards = eitherCards ^?! _Right
  let allDecks = getAllDecks cards
  let allCreatures = mapMaybe cardToCreature $ concat allDecks
  let board = exampleBoard cards
  let (turn, turn') = (initialTurn, nextTurn turn)
  describe "initial state is correct" $ do
    it "cards can be loaded from json" $
      is _Right eitherCards -- should be the first test, others depend on it
    it "all decks are initially of the same size" $
      all (\l -> length l == length (head allDecks)) allDecks
    it "all hit points are initially > 0" $
      all (\c -> hp c > 0) allCreatures
    it "all attacks are initially >= 0" $
      all (\c -> attack c >= 0) allCreatures
    it "welcome and board backgrounds agree in width" $
      boardPixelWidth `shouldBe` welcomePixelWidth
    it "welcome and board backgrounds agree in height" $
      boardPixelHeight `shouldBe` welcomePixelHeight
  describe "attack order contains all spots"
    $ it "check the lengths"
    $ length allCardsSpots `shouldBe` length attackOrder
  describe "AI.hs"
    $ xit "aiPlay terminates" -- xit: FIXME @smelc skipping test for now, aiPlay doesn't terminate
    $ all (is _Right . testAI board) [turn, turn']
