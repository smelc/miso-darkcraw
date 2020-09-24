{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import AI (aiPlay)
import Board
  ( Board,
    allCardsSpots,
    allPlayersSpots,
    boardToCardsInPlace,
    emptyInPlaceBoard,
    exampleBoard,
    inTheBack,
  )
import Card
import Cinema
import Constants
import Control.Lens
import Control.Lens.Extras
import Control.Monad
import Control.Monad.Except (runExcept)
import Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import Game (GamePlayEvent, attackOrder, playAll)
import Json
import Movie
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

-- XXX smelc group AI tests together

-- | Tests that the AI treats 'Ranged' correctly.
testAIRanged :: [Card UI] -> Turn -> Board Core
testAIRanged cards turn =
  case aiPlay board turn & runExcept of
    Left _ -> error "AI failed"
    Right events ->
      case playAll board events of
        Left _ -> error "AI failed"
        Right (board', _) -> board'
  where
    archer =
      CreatureCard
        $ unsafeCreatureWithID cards
        $ CreatureID Archer Undead
    board = emptyInPlaceBoard cards [archer]

testSceneInvariant :: (Int, Scene Display) -> Spec
testSceneInvariant (idx, Scene {..}) =
  -- Check no two Element are in the same spot
  it ("Scene Diff invariant " ++ show idx) $
    values `shouldBe` values'
  where
    stateToXY State {..} = (x, y)
    values = Map.filterWithKey isActor (unMappingType mapping) & Map.elems & List.sort
    values' = values & Set.fromList & Set.toList & List.sort
    isActor (Actor _ _) _ = True
    isActor (TileElement _) _ = False

testScenesInvariant :: String -> [Scene Diff] -> Spec
testScenesInvariant name diffs =
  describe ("Scene Diff " ++ name) $ do
    it "length([Scene Diff]) == length(display [Scene Diff])" $
      length
        diffs
        `shouldBe` length displays
    forM_ (zip [0 ..] displays) testSceneInvariant
  where
    displays = display diffs

testParallelSceneComposition :: Spec
testParallelSceneComposition =
  describe "Cinema.|||"
    $ it "interleaves events in the expected order"
    $ scenes1 ||| scenes2 `shouldBe` expectedMergedScenes
  where
    w0 :: Element
    w0 = Actor 0 $ CreatureID Skeleton Undead
    w1 :: Element
    w1 = Actor 1 $ CreatureID Skeleton Undead
    scenes1 :: [Scene Diff]
    scenes1 =
      [ while 1 (w0 =: Cinema.at 0 0),
        while 3 (w0 =: right),
        while 1 (w0 =: left)
      ]
    scenes2 :: [Scene Diff]
    scenes2 =
      [ while 2 (w1 =: Cinema.at 1 1),
        while 4 (w1 =: right)
      ]
    expectedMergedScenes :: [Scene Diff]
    expectedMergedScenes =
      [ while 1 (w0 =: Cinema.at 0 0 <> w1 =: Cinema.at 1 1),
        while 1 (w0 =: right),
        while 2 (w1 =: right),
        while 2 (w0 =: left)
      ]

main :: IO ()
main = hspec $ do
  let eitherCardsNTiles = loadJson
  let (cards, _) = eitherCardsNTiles ^?! _Right
  let allDecks = getAllDecks cards
  let allCreatures = mapMaybe cardToCreature $ concat allDecks
  let board = exampleBoard cards
  let (turn, turn') = (initialTurn, nextTurn turn)
  describe "initial state is correct" $ do
    it "cards can be loaded from json" $
      is _Right eitherCardsNTiles -- should be the first test, others depend on it
    it "all decks are initially of the same size" $
      all (\l -> length l == length (head allDecks)) allDecks
    it "all hit points are initially > 0" $
      all (\c -> hp c > 0) allCreatures
    it "all attacks are initially >= 0" $
      all (\c -> attack c >= 0) allCreatures
    it "lobbies and board backgrounds agree in width" $
      boardPixelWidth `shouldBe` lobbiesPixelWidth
    it "lobbies and board backgrounds agree in height" $
      boardPixelHeight `shouldBe` lobbiesPixelHeight
  describe "exactly all spots are used" $ it "attackOrder" $
    all
      (\pSpot -> length allCardsSpots == length (attackOrder pSpot))
      allPlayersSpots
  describe "AI.hs" $ do
    it "AI terminates" $
      all (is _Right . testAI board) [turn, turn']
    xit "AI puts Ranged creature in back line" $
      all
        (\(_, cSpot, _) -> inTheBack cSpot)
        (boardToCardsInPlace $ testAIRanged cards initialTurn)
  testScenesInvariant "welcomeMovie" welcomeMovie
  testParallelSceneComposition
