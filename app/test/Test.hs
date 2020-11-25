{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import AI
import Board
import Card
import Cinema
import Constants
import Control.Lens hiding (at, (+=))
import Control.Lens.Extras
import Control.Monad
import Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Game (GamePlayEvent (..), PlayTarget (..), attackOrder, playAll)
import Generators
import Json
import Movie
import Pretty
import SceneEquivalence
import SharedModel (SharedModel)
import qualified SharedModel
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Turn

creatureSum :: [Creature p] -> (Creature p -> Int) -> Int
creatureSum cards getter = sum (map getter cards)

getAllDecks :: [Card UI] -> [[Card Core]]
getAllDecks cards = [teamDeck cards t | t <- allTeams]

testBalance :: [Card UI] -> Int
testBalance cards =
  undefined
  where
    _humanDeck = teamDeck cards Human
    _undeadDeck = teamDeck cards Undead

-- XXX smelc group AI tests together

-- | Tests that the AI treats 'Ranged' correctly.
testAIRanged :: SharedModel -> Turn -> Board Core
testAIRanged shared turn =
  case playAll shared board events of
    Left _ -> error "AI failed"
    Right (board', _) -> board'
  where
    archer = IDC $ CreatureID Archer Undead
    board = boardAddToHand emptyBoard (turnToPlayerSpot turn) archer
    events = aiPlay SharedModel.unsafeGet board turn

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
  describe "Cinema.fork" $
    it "interleaves events as expected" $
      actualScene ~= expectedScene
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
  describe "Cinema.|||" $
    it "interleaves events in the expected order" $
      actualMergedScene ~= expectedMergedScene
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
  modifyMaxSize (const 35) $
    describe "Scene.return" $ do
      prop "left neutral for >>" $
        \(astToScene -> scene) ->
          (return () >> scene) ~= scene
      prop "right neutral for >>" $
        \(astToScene -> scene) ->
          (scene >> return ()) ~= scene
      prop "left neutral for |||" $
        \(astToScene -> scene) ->
          (return () ||| scene) ~= scene
      prop "right neutral for |||" $
        \(astToScene -> scene) ->
          (scene ||| return ()) ~= scene

testGetActorState =
  describe "getActorState" $
    it "should read the state of the actor" $
      actualScene ~= expectedScene
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

testAIPlace shared =
  describe "AI.placeCards" $ do
    prop "placeCards returns events whose spots differ" $
      \board turn -> allDiff $ AI.placeCards shared board turn
    prop "placeCards return events that commute" $
      \(Pretty board) (Pretty turn) ->
        let events = AI.placeCards shared board turn
         in length events >= 2
              ==> forAll (Test.QuickCheck.elements (permutations events))
              $ \events' ->
                Pretty (ignoreErrMsg (playAll shared board events)) `shouldBe` Pretty (ignoreErrMsg (playAll shared board events'))
  where
    ignoreErrMsg (Left _) = Nothing
    ignoreErrMsg (Right (board', _)) = Just board'
    spotsDiffer (Place' (CardTarget pSpot1 cSpot1) _) (Place' (CardTarget pSpot2 cSpot2) _) =
      pSpot1 /= pSpot2 || cSpot1 /= cSpot2
    spotsDiffer _ _ = error "Only Place' events should have been generated"
    allDiff [] = True
    allDiff (event : events) = all (spotsDiffer event) events && allDiff events

{- HLINT ignore testInPlaceEffectsMonoid -}
testInPlaceEffectsMonoid =
  describe "InPlaceEffects is a well behaved Monoid" $ do
    prop "mempty <> effects == effect" $
      \(effect :: InPlaceEffects) ->
        mempty <> effect `shouldBe` effect
    prop "effects <> mempty == effect" $
      \(effect :: InPlaceEffects) ->
        effect <> mempty `shouldBe` effect
    prop "effects <> effects' == effects' <> effects" $
      \(effect :: InPlaceEffects, effect') ->
        effect <> effect' `shouldBe` effect' <> effect

main :: IO ()
main = hspec $ do
  let eitherCardsNTiles = loadJson
  let (cards, _, _) = eitherCardsNTiles ^?! _Right
  let allDecks = getAllDecks cards
  let allCreatures = mapMaybe cardToCreature $ concat allDecks
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
  describe "exactly all spots are used" $
    it "attackOrder" $
      all
        (\pSpot -> length allCardsSpots == length (attackOrder pSpot))
        allPlayersSpots
  describe "AI.hs" $
    it "AI puts Ranged creature in back line" $
      let occupiedSpots =
            boardToHoleyInPlace (testAIRanged SharedModel.unsafeGet initialTurn)
              & filter (\(_, _, maybeCreature) -> isJust maybeCreature)
       in all (\(_, cSpot, _) -> inTheBack cSpot) occupiedSpots
            && not (null occupiedSpots)
  testScenesInvariant "welcomeMovie" welcomeMovie
  testParallelSceneComposition
  describe "Cinema.|||" $
    it "does not create artificial intermediary frames" $
      (wait 1 ||| wait 2) ~= wait 2
  testForkScene
  modifyMaxSize (const 35) $
    describe "Cinema.fork" $
      prop "should behave like ||| with rest of program" $
        \(astToScene -> scene1) (astToScene -> scene2) (astToScene -> scene3) ->
          (do scene1; fork scene2; scene3) ~= (do scene1; scene2 ||| scene3)
  modifyMaxSize (const 35) $
    describe "Cinema.>>" $
      prop "should be associative" $
        \(astToScene -> scene1) (astToScene -> scene2) (astToScene -> scene3) ->
          ((scene1 >> scene2) >> scene3) ~= (scene1 >> (scene2 >> scene3))
  modifyMaxSize (const 35) $
    describe "Cinema.|||" $
      prop "should be associative" $
        \(astToScene -> scene1) (astToScene -> scene2) (astToScene -> scene3) ->
          ((scene1 ||| scene2) ||| scene3) ~= (scene1 ||| (scene2 ||| scene3))
  testSceneReturn
  testGetActorState
  testAIPlace SharedModel.unsafeGet
  testInPlaceEffectsMonoid
