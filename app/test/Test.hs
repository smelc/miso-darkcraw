{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Control.Lens hiding ((+=), at)
import Control.Lens.Extras
import Control.Monad
import Control.Monad.Except (runExcept)
import Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import Game (GamePlayEvent, attackOrder, playAll)
import Generators
import Json
import Movie
import SceneEquivalence
import Test.Hspec
import Test.Hspec.QuickCheck
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

testSceneInvariant :: Int -> TimedFrame -> Spec
testSceneInvariant idx TimedFrame {..} =
  -- Check no two Element are in the same spot
  it ("Scene Change invariant " ++ show idx) $
    values `shouldBe` values'
  where
    values = unFrame frame & Map.elems & map actorState & filter isCreature & map toPos & List.sort
    values' = values & Set.fromList & Set.toList & List.sort
    isCreature ActorState {sprite = Just s} | spriteToKind s == CreatureKind = True
    isCreature _ = False
    toPos ActorState {x, y} = (x, y)

testScenesInvariant :: String -> Scene () -> Spec
testScenesInvariant name scene =
  describe ("Scene ActorChange " ++ name) $
    zipWithM_ testSceneInvariant [0 ..] (render scene)

testForkScene :: Spec
testForkScene =
  describe "Cinema.fork"
    $ it "interleaves events as expected"
    $ actualScene ~= expectedScene
  where
    scene1 :: Element -> Element -> Scene ()
    scene1 w0 w1 = do
      during 1 (up w0)
      fork $ do
        during 1 (down w1)
        during 1 (down w1)
        during 1 (down w1)
      during 1 (up w0)
    scene2 :: Element -> Scene ()
    scene2 w0 =
      during 1 (up w0)
    actualScene :: Scene ()
    actualScene = do
      w0 <- newHiddenActor "w0"
      w1 <- newHiddenActor "w1"
      scene1 w0 w1
      scene2 w0
    expectedScene :: Scene ()
    expectedScene = do
      w0 <- newHiddenActor "w0"
      w1 <- newHiddenActor "w1"
      during 1 (up w0)
      during 1 (do up w0; down w1)
      during 1 (do up w0; down w1)
      during 1 (down w1)

testParallelSceneComposition :: Spec
testParallelSceneComposition =
  describe "Cinema.|||"
    $ it "interleaves events in the expected order"
    $ actualMergedScene ~= expectedMergedScene
  where
    scene1 :: Element -> Scene ()
    scene1 w0 = do
      during 1 (w0 & moveTo 0 0)
      during 3 (right w0)
      during 1 (left w0)
    scene2 :: Element -> Scene ()
    scene2 w1 = do
      during 2 (w1 & moveTo 1 1)
      during 4 (right w1)
    actualMergedScene :: Scene ()
    actualMergedScene = do
      w0 <- newHiddenActor "w0"
      w1 <- newHiddenActor "w1"
      scene1 w0 ||| scene2 w1
    expectedMergedScene :: Scene ()
    expectedMergedScene = do
      w0 <- newHiddenActor "w0"
      w1 <- newHiddenActor "w1"
      during 1 (do w0 & moveTo 0 0; w1 & moveTo 1 1)
      during 1 (right w0)
      during 2 (right w1)
      during 2 (left w0)

{- HLINT ignore testSceneReturn -}
testSceneReturn :: SpecWith ()
testSceneReturn =
  modifyMaxSize (const 35) $ describe "Scene.return" $ do
    prop "left neutral for >>" $
      \ast ->
        let scene = astToScene ast
         in (return () >> scene) ~= scene
    prop "right neutral for >>" $
      \ast ->
        let scene = astToScene ast
         in (scene >> return ()) ~= scene
    prop "left neutral for |||" $
      \ast ->
        let scene = astToScene ast
         in (return () ||| scene) ~= scene
    prop "right neutral for |||" $
      \ast ->
        let scene = astToScene ast
         in (scene ||| return ()) ~= scene

testGetActorState =
  describe "getActorState"
    $ it "should read the state of the actor"
    $ actualScene ~= expectedScene
  where
    skeleton = creatureSprite $ CreatureID Skeleton Undead
    actualScene = do
      a1 <- newActorAt "a1" skeleton 1 2
      a1x <- a1 `dot` x
      a1y <- a1 `dot` y
      newActorAt "a2" skeleton a1x a1y
      wait 1
    expectedScene = do
      newActorAt "a1" skeleton 1 2
      newActorAt "a2" skeleton 1 2
      wait 1

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
  describe "Cinema.|||"
    $ it "does not create artificial intermediary frames"
    $ (wait 1 ||| wait 2) ~= wait 2
  testForkScene
  modifyMaxSize (const 35) $ describe "Cinema.fork"
    $ prop "should behave like ||| with rest of program"
    $ \ast1 ast2 ast3 ->
      let [scene1, scene2, scene3] = map astToScene [ast1, ast2, ast3]
       in (do scene1; fork scene2; scene3) ~= (do scene1; scene2 ||| scene3)
  modifyMaxSize (const 35) $ describe "Scene.>>"
    $ prop "should be associative"
    $ \ast1 ast2 ast3 ->
      let [scene1, scene2, scene3] = map astToScene [ast1, ast2, ast3]
       in ((scene1 >> scene2) >> scene3) ~= (scene1 >> (scene2 >> scene3))
  testSceneReturn
  testGetActorState
