{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import AI
import qualified Balance
import Board
import Card
import Cinema
import Command
import Constants
import Control.Lens hiding (at, (+=))
import Control.Lens.Extras
import Control.Monad
import Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace (traceShow)
import Game (Target (..), applyPlague, cardsToDraw, drawCards)
import qualified Game (Event (..), PolyResult (..), attackOrder, play, playAll)
import Generators
import qualified Invariants
import Json
import qualified Match
import Movie
import Pretty
import SceneEquivalence
import SharedModel (SharedModel, idToCreature)
import qualified SharedModel
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Total
import Turn

creatureSum :: [Creature p] -> (Creature p -> Int) -> Int
creatureSum cards getter = sum (map getter cards)

getAllDecks :: [Card UI] -> [[Card Core]]
getAllDecks cards = [teamDeck cards t | t <- allTeams]

-- | Tests that the AI treats 'Ranged' correctly.
testAIRanged :: SharedModel -> Turn -> Board Core
testAIRanged shared turn =
  case Game.playAll shared board events of
    Left _ -> error "AI failed"
    Right (Game.Result _ board' () _) -> board'
  where
    (t, teams) = (Undead, Teams Undead Undead)
    archer = IDC (CreatureID Archer t) []
    pSpot = Turn.toPlayerSpot turn
    board = boardAddToHand (emptyBoard teams) pSpot archer
    events = AI.play shared board pSpot

-- | This test was written in the hope it would reveal why
-- there are such logs in the console when playing the last card of the hand:
-- "Invalid hand index: 4. Hand has 4 card(s)."
-- Unfortunately, the test passes ^^ so it's a UI only bug. It's a good
-- test though, so I kept it.
testPlayLastHandCard :: SharedModel -> SpecWith ()
testPlayLastHandCard shared =
  describe
    "Play the last card of the hand"
    $ prop "doesn't yield an error" $
      \(Pretty board, Pretty turn) ->
        let event = playLastCardEvent board $ Turn.toPlayerSpot turn
         in isJust event
              ==> Game.play shared board (fromJust event) `shouldSatisfy` isRight
  where
    -- Very simple AI-like function
    playLastCardEvent board pSpot =
      case lastCardIdx board pSpot of
        Nothing -> Nothing
        Just i ->
          let (id, targetKind) = (boardToHand board pSpot !! i, targetType id)
           in case targetKind of
                PlayerTargetType -> Just $ Game.Place pSpot (PlayerTarget pSpot) $ HandIndex i -- Choosing pSpot is arbitrary
                CardTargetType ctk ->
                  boardToPlayerCardSpots board pSpot ctk
                    & listToMaybe
                    <&> (\cSpot -> Game.Place pSpot (CardTarget pSpot cSpot) $ HandIndex i)
    lastCardIdx board pSpot =
      case boardToHand board pSpot of
        [] -> Nothing
        l -> Just $ length l - 1
    isRight (Right _) = True
    isRight (Left _) = False

-- Tests that playing a creature only affects the target spot. Some
-- creatures are omitted: the ones with Discipline, because it breaks this
-- property. This test is not plugged yet because it breaks: if you
-- play a creature close to a creature with Discipline, the creature
-- with Discipline is affected (whereas it shouldn't be!).
testPlayFraming :: SharedModel -> SpecWith ()
testPlayFraming shared =
  describe
    "Playing some cards doesn't change"
    $ prop "other spots of the board when placing non-Discipline creature" $
      \(Pretty board, Pretty turn) ->
        let pSpot = Turn.toPlayerSpot turn
         in let pair = pickCardSpot board 0 pSpot
             in isJust pair
                  ==> let (i, cSpot) = fromJust pair
                       in Game.play shared board (Game.Place pSpot (Game.CardTarget pSpot cSpot) (HandIndex i))
                            `shouldSatisfy` relation board pSpot cSpot
  where
    -- Very simple AI-like function
    pickCardSpot (board :: Board 'Core) i pSpot =
      case boardToHand board pSpot of
        [] -> Nothing
        id : hand' ->
          if breaksFraming id
            then pickCardSpot (boardSetHand board pSpot hand') (i + 1) pSpot -- try next card
            else case targetType id of
              PlayerTargetType -> Nothing
              CardTargetType ctk ->
                boardToPlayerCardSpots board pSpot ctk
                  & listToMaybe
                  <&> (i,)
    breaksFraming (IDC id items) =
      SharedModel.idToCreature shared id items
        & fromJust
        & Card.unliftCreature
        & Total.isDisciplined
    breaksFraming _ = False
    relation _ _ _ (Left _) = True
    relation board pSpot cSpot (Right (Game.Result _ board' _ _)) = boardEq board pSpot [cSpot] board'
    boardEq (board :: Board 'Core) pSpot cSpots board' =
      let otherSpot = otherPlayerSpot pSpot
       in -- Board must be completely equivalent on part of player that doesn't play
          (boardToPart board otherSpot == boardToPart board' otherSpot)
            -- and agree w.r.t. partEq on the part of the player that played
            && partEq (boardToPart board pSpot) (boardToPart board' pSpot) cSpots
    partEq
      (PlayerPart {..} :: PlayerPart 'Core)
      (PlayerPart {inPlace = inPlace', score = score', stack = stack', discarded = discarded', team = team'} :: PlayerPart 'Core)
      cSpots =
        score == score' && stack == stack' && discarded == discarded' && team == team'
          && (Map.withoutKeys inPlace (Set.fromList cSpots) == Map.withoutKeys inPlace' (Set.fromList cSpots))

testDrawCards :: SharedModel -> SpecWith ()
testDrawCards shared =
  describe
    "Drawing cards"
    $ prop "draws the expected number" $
      \(Pretty board, Pretty turn) ->
        let pSpot = Turn.toPlayerSpot turn
         in let srcs = Game.cardsToDraw board pSpot True
             in Game.drawCards shared board pSpot srcs `shouldSatisfy` cond board pSpot
  where
    cond _ _ (Left errMsg) = traceShow errMsg False
    cond board pSpot (Right (_, board', _)) =
      (len' - len)
        == min (Constants.nbCardsToDraw + nbCardDrawSkills) (boardToStack board pSpot & length)
      where
        len = boardToHand board pSpot & length
        len' = boardToHand board' pSpot & length
        nbCardDrawSkills =
          boardToPlayerHoleyInPlace board pSpot
            & map snd
            & catMaybes
            & map skills
            & concat
            & filter (\case DrawCard' b -> b; _ -> False)
            & length

testShared shared =
  describe "SharedModel" $ do
    prop "maps all Item values" $
      \item ->
        let found =
              SharedModel.getCards shared
                & map (\case ItemCard ItemObject {item = i} | item == i -> Just i; _ -> Nothing)
                & catMaybes
         in found `shouldBe` [item]
    prop "maps all Neutral values" $
      \n ->
        let found =
              SharedModel.getCards shared
                & map (\case NeutralCard NeutralObject {neutral = n'} | n == n' -> Just n'; _ -> Nothing)
                & catMaybes
         in found `shouldBe` [n]

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
    allStrings = map show allCommands & sort
    uniqueStrings = allStrings & Set.fromList & Set.toList & sort

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
    prop "placeCards returns events whose card is valid" $
      \board pSpot -> AI.placeCards shared board pSpot `shouldSatisfy` (goodCards board pSpot)
    prop "placeCards returns events playing cards of the player whose turn it is" $
      \board pSpot -> AI.placeCards shared board pSpot `shouldSatisfy` playerIs pSpot
    prop "placeCards return events that commute (modulo Discipline)" $
      \(Pretty board) (Pretty turn) ->
        let events = AI.placeCards shared board turn & filter (not . hasDiscipline)
         in (length events >= 2)
              ==> forAll (Test.QuickCheck.elements (permutations events))
              $ \events' ->
                Pretty (ignoreErrMsg (Game.playAll shared board events)) `shouldBe` Pretty (ignoreErrMsg (Game.playAll shared board events'))
  where
    ignoreErrMsg (Left _) = Nothing
    ignoreErrMsg (Right (Game.Result _ board' () _)) = Just board'
    spotsDiffer (Game.Place' _ (Game.CardTarget pSpot1 cSpot1) _) (Game.Place' _ (Game.CardTarget pSpot2 cSpot2) _) =
      pSpot1 /= pSpot2 || cSpot1 /= cSpot2
    spotsDiffer _ _ = error "Only Place' events should have been generated"
    allDiff [] = True
    allDiff (event : events) = all (spotsDiffer event) events && allDiff events
    hasDiscipline (Game.Place' _ _ (IDC id items)) =
      SharedModel.idToCreature shared id items
        & fromJust
        & Card.unliftCreature
        & Total.isDisciplined
    hasDiscipline (Game.Place' _ _ _) = False
    hasDiscipline _ = error "Only Place' events should have been generated"
    goodCards _ _ [] = True
    goodCards board pSpot (Game.Place _ _ HandIndex {unHandIndex = i} : tl) =
      case (0 <= i, i < handSize) of
        (False, _) -> traceShow ("Wrong hand index: " ++ show i) False
        (_, False) -> traceShow ("Wrong hand index: " ++ show i ++ ", hand has " ++ show handSize ++ " members.") False
        _ -> goodCards board pSpot tl
      where
        handSize = List.length $ boardToHand board pSpot
    goodCards (board :: Board 'Core) pSpot (Game.Place' _ _ id : tl) =
      if id `elem` hand
        then goodCards board pSpot tl
        else traceShow ("Wrong ID: " ++ show id ++ "It does not belong to the hand: " ++ show hand) False
      where
        hand = boardToHand board pSpot
    goodCards boards pSpot (_ : tl) = goodCards boards pSpot tl
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
      \(effect :: InPlaceEffects) ->
        mempty <> effect `shouldBe` effect
    prop "effects <> mempty == effect" $
      \(effect :: InPlaceEffects) ->
        effect <> mempty `shouldBe` effect
    prop "effects <> effects' == effects' <> effects (modulo fadeOut)" $
      \(effect :: InPlaceEffects, effect') ->
        let (e1, e2) = rmCommonFadeout effect effect'
         in e1 <> e2 `shouldBe` e1 <> e2
  where
    hasCommonFadeout effects1 effects2 =
      any
        ( \cSpot ->
            nonEmptyFadeout ((unInPlaceEffects effects1) Map.!? cSpot)
              && nonEmptyFadeout ((unInPlaceEffects effects2) Map.!? cSpot)
        )
        allCardsSpots
    nonEmptyFadeout Nothing = False
    nonEmptyFadeout (Just effect) = not $ null $ fadeOut effect
    rmCommonFadeout e1 e2 | hasCommonFadeout e1 e2 = (rmFadeout e1, e2)
    rmCommonFadeout e1 e2 = (e1, e2)
    rmFadeout InPlaceEffects {unInPlaceEffects = effects} =
      Map.map (\effect -> effect {fadeOut = []}) effects & InPlaceEffects

testNoPlayEventNeutral shared =
  describe "Game.NoPlayEvent is neutral w.r.t Game.playAll" $
    prop "Game.playAll $ [NoPlayEvent] ++ es == Game.playAll $ es ++ [NoPlayEvent]" $
      \(board, events) ->
        let left = [Game.NoPlayEvent] ++ events
         in let right = events ++ [Game.NoPlayEvent]
             in Game.playAll shared board left `shouldBe` Game.playAll shared board right

testPlayScoreMonotonic shared =
  describe "boardScore is monotonic w.r.t. Game.play" $
    prop "forall b :: Board, let b' = Game.play b (AI.aiPlay b); score b' is better than score b" $
      \(board, pSpot) ->
        let score = flip boardScore pSpot
         in let (initialScore, events) = (score board, AI.play shared board pSpot)
             in let nextScore = Game.playAll shared board events & takeBoard <&> score
                 in monotonic initialScore nextScore
  where
    takeBoard (Left _) = Nothing
    takeBoard (Right (Game.Result _ b _ _)) = Just b
    monotonic _ Nothing = True -- Nothing to test
    monotonic i (Just j) = j <= i -- Better score is smaller score

testFear shared =
  describe "Fear works as expected" $ do
    it "fear triggers when expected" $
      (boardToPart board'' PlayerBot & Board.inPlace) `shouldSatisfy` Map.null
    it "fear kills go to discarded stack" $
      (boardToDiscarded board'' PlayerBot `shouldBe` [IDC fearTarget []])
    it "fear does not trigger when expected" $
      (boardToPart boardBis'' PlayerBot & Board.inPlace) `shouldNotSatisfy` Map.null
  where
    teams = Teams Undead Human
    causingFear = CreatureID Skeleton Undead
    fearTarget = CreatureID Archer Human
    board = smallBoard shared teams causingFear [] PlayerTop Bottom
    affectedByFear =
      SharedModel.idToCreature shared fearTarget []
        & fromJust
        & unliftCreature
    board' =
      boardSetCreature
        board
        PlayerBot
        (bottomSpotOfTopVisual Top)
        (affectedByFear & (\Creature {hp, ..} -> Creature {hp = 1, ..}))
    board'' = Game.play shared board' (Game.ApplyFearNTerror PlayerTop) & extract
    extract (Left _) = error "Test failure"
    extract (Right (Game.Result _ b _ _)) = b
    -- Second test
    boardBis' =
      boardSetCreature
        board
        PlayerBot
        (bottomSpotOfTopVisual Top)
        affectedByFear
    boardBis'' = Game.play shared boardBis' (Game.ApplyFearNTerror PlayerTop) & extract

testFearNTerror shared =
  describe "Fear and terror" $ do
    prop "Creature causing fear is immune to fear" $
      \c -> Total.causesFear c ==> not $ Total.affectedByFear c
    prop "Creature causing terror is immune to fear" $
      \c -> Total.causesTerror c ==> not $ Total.affectedByFear c
    prop "Creature causing terror is immune to terror" $
      \c -> Total.causesTerror c ==> not $ Total.affectedByTerror c
    modifyMaxDiscardRatio (* 5) $
      prop "Creature affected by fear is affected by terror" $
        \c -> Total.affectedByFear c ==> Total.affectedByTerror c

testPlague shared =
  describe "Plague" $ do
    prop "Plague kills creatures with hp <= 1" $
      \(teams :: Teams, cids :: [(CreatureID, [Item])]) ->
        applyPlague (setHp 1) teams cids `shouldSatisfy` Map.null
    prop "Plague doesn't kill creatures with hp > 1" $
      \(teams :: Teams, cids :: [(CreatureID, [Item])]) ->
        (applyPlague (setHp 2) teams cids & Map.size) `shouldBe` min 6 (length cids)
  where
    addAllToInPlace b pSpot cids = foldr (\pair b -> addToInPlace b pSpot pair) b cids
    addToInPlace (b :: Board 'Core) pSpot (cid, items) =
      case firstEmpty of
        Nothing -> b
        Just cSpot ->
          let creature = SharedModel.idToCreature shared cid items & fromJust & Card.unliftCreature
           in boardSetInPlace b pSpot (boardToInPlace b pSpot & Map.insert cSpot creature)
      where
        firstEmpty =
          boardToPlayerHoleyInPlace b pSpot
            & filter (\(_, m) -> isNothing m)
            & listToMaybe
            <&> fst
    mkBoard teams pSpot cids = addAllToInPlace (emptyBoard teams) pSpot cids
    mapInPlace f pSpot (board :: Board 'Core) =
      boardToInPlace board pSpot
        & Map.map f
        & boardSetInPlace board pSpot
    setHp i c = c {hp = i}
    applyPlague f teams cids =
      Game.applyPlague board pSpot & flip boardToInPlace pSpot
      where
        pSpot = PlayerTop
        board = mkBoard teams pSpot cids & mapInPlace f pSpot

main :: IO ()
main = hspec $ do
  let eitherCardsNTiles = loadJson
  let (cards, _, _, _) = eitherCardsNTiles ^?! _Right
  let allDecks = getAllDecks cards
  let allCreatures = mapMaybe cardToCreature $ concat allDecks
  describe "initial state is correct" $ do
    it "cards can be loaded from json" $
      is _Right eitherCardsNTiles -- should be the first test, others depend on it
    xit "all decks are initially of the same size (modulo items)" $
      let itemLessDecks = map (filter (\case ItemCard _ -> False; _ -> True)) allDecks
       in all (\l -> length l == length (head itemLessDecks)) itemLessDecks
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
        (\pSpot -> length allCardsSpots == length (Game.attackOrder pSpot))
        allPlayersSpots
  let shared = SharedModel.unsafeGet
  describe "AI.hs" $
    it "AI puts Ranged creature in back line" $
      let occupiedSpots =
            boardToHoleyInPlace (testAIRanged shared Turn.initial)
              & filter (\(_, _, maybeCreature) -> isJust maybeCreature)
       in all (\(_, cSpot, _) -> inTheBack cSpot) occupiedSpots
            && not (null occupiedSpots)
  -- From fast tests to slow tests (to maximize failing early)
  testFear shared
  testFearNTerror shared
  testPlague shared
  testPlayFraming shared
  testDrawCards shared
  testShared shared
  testAIPlace shared
  testInPlaceEffectsMonoid
  testNoPlayEventNeutral shared
  testPlayScoreMonotonic shared
  testPlayLastHandCard shared
  Invariants.main shared
  Match.main shared
  Balance.main shared
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
