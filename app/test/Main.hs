{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

import AI
import qualified Art
import qualified Balance
import qualified Board
import qualified Campaign
import Card
import Cinema
import qualified Command
import Constants
import Control.Category ((>>>))
import Control.Monad.Except
import Data.Either
import Data.Either.Extra
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Tuple.Extra (both)
import Debug.Trace (trace, traceShow)
import GHC.Base (assert)
import Game (Target (..))
import qualified Game hiding (Playable (..))
import Generators
import qualified HeuristicAI
import qualified Invariants
import Json
import qualified Logic (main, mkCreature)
import qualified Mana
import qualified Match
import qualified Model
import Movie
import Nat
import Pretty
import SceneEquivalence
import qualified Shared
import qualified Skill
import Spots hiding (Card)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import TestLib (shouldAllSatisfy, shouldSatisfyRight)
import qualified Turn
import qualified Update

getAllDecks :: [Card 'UI] -> [[Card 'Core]]
getAllDecks cards = [teamDeck cards t | t <- allTeams]

-- | Tests that the AI treats 'Ranged' correctly.
testAIRanged :: Shared.Model -> Turn.T -> Board.T 'Core
testAIRanged shared turn =
  case Game.playAll shared (Game.mkPlayable board (map Game.PEvent events) turn) of
    Left _ -> error "AI failed"
    Right (Game.Result {board = board'}) -> board'
  where
    (t, teams) = (Undead, Board.Teams Undead Undead)
    archer = IDC (CreatureID Archer t) []
    pSpot = Turn.toPlayerSpot turn
    board = Board.addToHand (Board.empty teams) pSpot archer
    events = AI.play Constants.Easy shared board pSpot turn

testShared shared =
  describe "Shared.Model" $ do
    prop "maps all Item values" $
      \item ->
        let found =
              Shared.getCards shared
                & mapMaybe (\case ItemCard _ ItemObject {item = i} | item == i -> Just i; _ -> Nothing)
         in found `shouldBe` [item]
    prop "maps all Neutral values" $
      \n ->
        let found =
              Shared.getCards shared
                & mapMaybe (\case NeutralCard _ NeutralObject {neutral = n'} | n == n' -> Just n'; _ -> Nothing)
         in found `shouldBe` [n]
    it "All Item cards specify a text" $
      ( Shared.getCards shared
          & mapMaybe (\case ItemCard CardCommon {text} _ -> Just text; _ -> Nothing)
      )
        `shouldAllSatisfy` isJust
    it "All Neutral cards specify a text" $
      ( Shared.getCards shared
          & mapMaybe (\case NeutralCard CardCommon {text} _ -> Just text; _ -> Nothing)
      )
        `shouldAllSatisfy` isJust
    it "Text and skills are exclusive in Creature cards" $
      ( Shared.getCards shared
          & mapMaybe (\case CreatureCard CardCommon {text} Creature {skills} -> Just (text, skills); _ -> Nothing)
      )
        `shouldAllSatisfy` ( \case
                               (Nothing, _skills) -> True -- no text
                               (Just _text, []) -> True -- no skills
                               (Just _text, _skills) -> False -- both text and some skills
                           )

testNeighbors :: SpecWith ()
testNeighbors =
  describe "Neighbors doesn't return doublons" $
    prop "forall n::Neighborhood cSpot::CardSpot, neighbors n cSpot doesn't have doublons" $
      \(n, cSpot) ->
        let res = neighbors n cSpot
         in (res & sort) `shouldBe` (res & Set.fromList & Set.toList & sort)

testShowCommands :: SpecWith ()
testShowCommands =
  describe "Show Command" $
    it "is injective" $
      allStrings `shouldBe` uniqueStrings
  where
    allStrings = map show (Command.allCommands cids) & sort
    uniqueStrings = allStrings & Set.fromList & Set.toList & sort
    cids = [CreatureID kind team | kind <- allCreatureKinds, team <- allTeams]

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
      _ <- newActorAt "a2" skeleton a1x a1y
      wait 1
    expectedScene = do
      _ <- newActorAt "a1" skeleton 1 2
      _ <- newActorAt "a2" skeleton 1 2
      wait 1

testAIPlace shared =
  describe "AI.placeCards" $ do
    prop "placeCards returns events whose spots differ on Creature-only hand" $
      \board (pSpot :: Spots.Player) (cidNItems :: [(CreatureID, [Item])]) ->
        -- Only use Creature cards. Items and neutrals obviously break this property
        let board' = Board.setHand board pSpot (map (uncurry Card.IDC) cidNItems)
         in allDiff $ play board' pSpot
    prop "placeCards returns events whose card is valid" $
      \board pSpot -> play board pSpot `shouldSatisfy` (goodCards board pSpot)
    prop "placeCards returns events playing cards of the player whose turn it is" $
      \board pSpot -> play board pSpot `shouldSatisfy` playerIs pSpot
    prop "playHand's first card can always be played successfully" $
      \(Pretty (board :: Board.T 'Core)) (Pretty pSpot) ->
        let place = play board pSpot & listToMaybe
         in isJust place
              ==> (Game.maybePlay shared (Game.mkPlayable board (Game.PEvent (fromJust place)) turn))
                `shouldSatisfy` isJust
  where
    play b p = AI.play difficulty shared b p turn
    turn = Turn.initial
    difficulty = Constants.Easy
    spotsDiffer (Game.Place' _ (Game.CardTarget pSpot1 cSpot1) _) (Game.Place' _ (Game.CardTarget pSpot2 cSpot2) _) =
      pSpot1 /= pSpot2 || cSpot1 /= cSpot2
    spotsDiffer _ _ = error "Only Place' events should have been generated"
    allDiff [] = True
    allDiff (event : events) = all (spotsDiffer event) events && allDiff events
    goodCards _ _ [] = True
    goodCards board pSpot (Game.Place _ _ Board.HandIndex {unHandIndex = i} : tl) =
      case (0 <= i, i < handSize) of
        (False, _) -> traceShow ("Wrong hand index: " ++ show i) False
        (_, False) -> traceShow ("Wrong hand index: " ++ show i ++ ", hand has " ++ show handSize ++ " members.") False
        _ -> goodCards board pSpot tl
      where
        handSize = List.length $ Board.toHand board pSpot
    goodCards (board :: Board.T 'Core) pSpot (Game.Place' _ _ id : tl) =
      if id `elem` hand
        then goodCards board pSpot tl
        else traceShow ("Wrong ID: " ++ show id ++ "It does not belong to the hand: " ++ show hand) False
      where
        hand = Board.toHand board pSpot
    playerIs _ [] = True
    playerIs expected (event : tl) = playerIs' expected event && playerIs expected tl
    playerIs' expected =
      \case
        Game.Place actual _ _ | expected /= actual -> False
        Game.Place' actual _ _ | expected /= actual -> False
        _ -> True

{- HLINT ignore testInPlaceEffectsMonoid -}
testInPlaceEffectsMonoid =
  describe "InPlaceEffects is a well behaved Monoid" $ do
    prop "mempty <> effects == effect" $
      \(effect :: Board.InPlaceEffects) ->
        mempty <> effect `shouldBe` effect
    prop "effects <> mempty == effect" $
      \(effect :: Board.InPlaceEffects) ->
        effect <> mempty `shouldBe` effect
    prop "effects <> effects' == effects' <> effects (modulo fadeOut)" $
      \(effect :: Board.InPlaceEffects, effect') ->
        let (e1, e2) = rmCommonFadeout effect effect'
         in e1 <> e2 `shouldBe` e1 <> e2
  where
    hasCommonFadeout effects1 effects2 =
      any
        ( \cSpot ->
            nonEmptyFadeout (effects1 Map.!? cSpot)
              && nonEmptyFadeout (effects2 Map.!? cSpot)
        )
        Spots.allCards
    nonEmptyFadeout Nothing = False
    nonEmptyFadeout (Just effect) = not $ null $ Board.fadeOut effect
    rmCommonFadeout e1 e2 | hasCommonFadeout e1 e2 = (rmFadeout e1, e2)
    rmCommonFadeout e1 e2 = (e1, e2)
    rmFadeout effects =
      Map.map (\effect -> effect {Board.fadeOut = []}) effects

testPlayScoreMonotonic shared =
  describe "boardScore is monotonic w.r.t. Game.play" $
    xit "forall b :: Board, let b' = Game.play b (AI.aiPlay b); score b' is better than score b" $
      property $
        \(board, pSpot, turn) ->
          let score = flip HeuristicAI.boardPlayerScore pSpot
              initialScore = score board
              events = AI.play Constants.Easy shared board pSpot turn
              nextScore =
                Game.playAll shared (Game.mkPlayable board (map Game.PEvent events) turn)
                  & takeBoard <&> score
           in monotonic initialScore nextScore
  where
    takeBoard (Left _) = Nothing
    takeBoard (Right (Game.Result {board = b})) = Just b
    monotonic _ Nothing = True -- Nothing to test
    monotonic i (Just j) = j <= i -- Better score is smaller score

testRewards =
  describe "Rewards work as expected" $ do
    it "initialisation" $ do
      Campaign.augment [] Campaign.Level0 Human == [[]]
    -- TODO @smelc, test Evil, Sylvan too
    prop "one card is received at every reward (except Evil, Sylvan, ZKnights)" $ do
      \(level, team) ->
        acceptable team
          ==> Campaign.augment [] level team
          `shouldAllSatisfy` (\deck -> natLength deck == Campaign.nbRewards level)
    -- TODO @smelc, test Evil, Sylvan too
    prop "There is always at least one reward (except Evil, Sylvan, ZKnights)" $ do
      \(outcome, level, team) ->
        acceptable team
          ==> Campaign.loot Nothing outcome level team `shouldSatisfy` (not . null)
  where
    -- FIXME @smelc, write a custom generator
    acceptable Evil = False
    acceptable Sylvan = False
    acceptable ZKnights = False
    acceptable _ = True

testItemsAI shared =
  describe "AI" $ do
    it "Sword of Might is put on most potent in place creature" $
      play (board1 (mkCreature' Archer Undead) (mkCreature' Vampire Undead) SwordOfMight)
        `shouldSatisfyRight` (\(Game.Result {board = board'}) -> hasItem board' pSpot Bottom SwordOfMight)
  where
    pSpot = PlayerTop
    board1 id1 id2 item =
      Board.empty teams
        & Board.insert pSpot TopLeft id1
        & Board.insert pSpot Bottom id2
        & (\b -> Board.addToHand b pSpot (IDI item))
    teams = Board.Teams Undead Undead
    mkCreature' kind team = Logic.mkCreature shared kind team False
    play board =
      Game.playAll
        shared
        (Game.mkPlayable board (map Game.PEvent (AI.play Constants.Easy shared board pSpot turn)) turn)
    turn = Turn.initial
    hasItem board pSpot cSpot item =
      (Board.toInPlaceCreature board pSpot cSpot) `has` item

testAIImprecise shared =
  describe "AI" $ do
    it "Imprecise card is put in back line" $
      (Game.playAll shared (Game.mkPlayable board (map Game.PEvent (AI.play Constants.Easy shared board pSpot turn)) turn))
        `shouldSatisfyRight` ( \(Game.Result {board = board'}) ->
                                 Board.toPlayerCardSpots board' pSpot Occupied
                                   & (\spots -> length spots == 1 && all inTheBack spots)
                             )
  where
    pSpot = PlayerTop
    (team, teams) = (ZKnights, Board.Teams team team)
    board = Board.empty teams & (\b -> Board.addToHand b pSpot trebuchet)
    trebuchet = Card.IDC (CreatureID Trebuchet team) []
    turn = Turn.initial

testMana shared =
  describe "AI" $ do
    prop "only play cards for which enough mana is available" $
      \(board, pSpot, turn) ->
        let manaAvailable = Board.toPart board pSpot & Board.mana
         in AI.play Easy shared board pSpot turn `shouldAllSatisfy` manaCostGeq board turn manaAvailable
  where
    -- manaCostGeq returns True if 'avail' is greater or equal to the
    -- mana cost of 'card'.
    manaCostGeq :: Board.T 'Core -> Turn.T -> Nat -> Game.Place -> Bool
    manaCostGeq board turn avail =
      \case
        Game.Place pSpot _ hi -> go' pSpot hi
        Game.Place' pSpot _ id ->
          case Game.idToHandIndex board pSpot id of
            Nothing -> traceShow ("Card not found:" ++ show id) False
            Just hi -> go' pSpot hi
      where
        go' pSpot hi =
          case go pSpot hi of
            Left errMsg -> traceShow errMsg False
            _ -> True
        go pSpot Board.HandIndex {unHandIndex = i} = do
          id :: Card.ID <- card
          card :: Card 'UI <-
            Shared.identToCard shared id
              & (\case Nothing -> Left "card not found"; Just x -> Right x)
          return ((Mana.<=) turn (Card.toCommon card & Card.mana) avail)
          where
            card :: Either Text Card.ID =
              Board.lookupHand (Board.toHand board pSpot) i

testApplyDifficulty stdgen =
  describe "applyDifficulty" $ do
    prop "Returned lists are permutations of the input list" $
      \(difficulty, SmallList (l :: [Int])) ->
        let perms = permutations l
         in HeuristicAI.applyDifficulty difficulty stdgen l
              `shouldAllSatisfy` (`elem` perms)

main :: IO ()
main = hspec $ do
  let eitherCardsNTiles = loadJson
  let cards :: [Card 'UI] = case eitherCardsNTiles of Left errMsg -> error errMsg; Right (x, _, _) -> x
  let allDecks = getAllDecks cards
  let allCreatures = mapMaybe cardToCreature $ concat allDecks
  describe "initial state is correct" $ do
    it "cards can be loaded from json" $
      isRight eitherCardsNTiles -- Should be the first test, others depend on it
    xit "all decks are initially of the same size (modulo items)" $
      let itemLessDecks = map (filter (\case ItemCard {} -> False; _ -> True)) allDecks
       in all (\l -> length l == length (head itemLessDecks)) itemLessDecks
    it "all hit points are initially > 0" $
      all (\c -> hp c > 0) allCreatures
    it "all attacks are initially >= mempty" $
      all (\c -> attack c >= mempty) allCreatures
    it "lobbies and board backgrounds agree in width" $
      boardPixelWidth `shouldBe` lobbiesPixelWidth
    it "lobbies and board backgrounds agree in height" $
      boardPixelHeight `shouldBe` lobbiesPixelHeight
  describe "exactly all spots are used" $
    it "attackOrder" $
      all
        (\pSpot -> length Spots.allCards == length (Game.attackOrder pSpot))
        Spots.allPlayers
  let shared = Shared.unsafeGet
  describe "AI.hs" $
    it "AI puts Ranged creature in back line" $
      let occupiedSpots =
            Board.toHoleyInPlace (testAIRanged shared Turn.initial)
              & filter (\(_, _, maybeCreature) -> isJust maybeCreature)
       in all (\(_, cSpot, _) -> inTheBack cSpot) occupiedSpots
            && not (null occupiedSpots)
  -- From fast tests to slow tests (to maximize failing early)
  -- Unit tests
  testItemsAI shared
  testShowCommands
  testAIImprecise shared
  -- PBT tests
  testMana shared
  testNeighbors
  testRewards
  testShared shared
  testAIPlace shared
  testInPlaceEffectsMonoid
  testApplyDifficulty $ Shared.getStdGen shared
  testPlayScoreMonotonic shared
  -- Onto other files
  Invariants.main shared
  Logic.main shared
  Match.main shared
  Balance.main shared False
  -- Onto tests of scenes
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
