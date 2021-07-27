{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import AI
import qualified Balance
import Board
import qualified Campaign
import Card
import Cinema
import Command
import Constants
import Control.Lens hiding (at, (+=))
import Control.Lens.Extras
import Control.Monad.Except
import Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import Debug.Trace (traceShow)
import Game (Target (..), applyPlague, cardsToDraw, drawCards, idToHandIndex)
import qualified Game (Event (..), PolyResult (..), attackOrder, play, playAll)
import Generators
import qualified Invariants
import Json
import qualified Match
import Movie
import Nat
import Pretty
import SceneEquivalence
import SharedModel (SharedModel, idToCreature)
import qualified SharedModel
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import TestLib (shouldAllSatisfy, shouldSatisfyJust)
import qualified Total
import Turn

creatureSum :: [Creature p] -> (Creature p -> Int) -> Int
creatureSum cards getter = sum (map getter cards)

getAllDecks :: [Card 'UI] -> [[Card 'Core]]
getAllDecks cards = [teamDeck cards t | t <- allTeams]

-- | Tests that the AI treats 'Ranged' correctly.
testAIRanged :: SharedModel -> Turn -> Board 'Core
testAIRanged shared turn =
  case Game.playAll shared board events of
    Left _ -> error "AI failed"
    Right (Game.Result _ board' () _) -> board'
  where
    (t, teams) = (Undead, Teams Undead Undead)
    archer = IDC (CreatureID Archer t) []
    pSpot = Turn.toPlayerSpot turn
    board = Board.addToHand (Board.empty teams) pSpot archer
    events = AI.play AI.Easy shared board pSpot

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
      case Board.toHand board pSpot of
        [] -> Nothing
        id : hand' ->
          if breaksFraming id
            then pickCardSpot (Board.setHand board pSpot hand') (i + 1) pSpot -- try next card
            else case targetType id of
              PlayerTargetType -> Nothing
              CardTargetType ctk ->
                Board.toPlayerCardSpots board pSpot ctk
                  & listToMaybe
                  <&> (i,)
    breaksFraming (IDC id items) =
      SharedModel.idToCreature shared id items
        & fromJust
        & Card.unlift
        & Total.isDisciplined
    breaksFraming _ = False
    relation _ _ _ (Left _) = True
    relation board pSpot cSpot (Right (Game.Result _ board' _ _)) = boardEq board pSpot [cSpot] board'
    boardEq (board :: Board 'Core) pSpot cSpots board' =
      let otherSpot = otherPlayerSpot pSpot
       in -- Board must be completely equivalent on part of player that doesn't play
          (Board.toPart board otherSpot == Board.toPart board' otherSpot)
            -- and agree w.r.t. partEq on the part of the player that played
            && partEq (Board.toPart board pSpot) (Board.toPart board' pSpot) cSpots
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
        == min (Constants.nbCardsToDraw + nbCardDrawSkills) (Board.toStack board pSpot & length)
      where
        len = Board.toHand board pSpot & length
        len' = Board.toHand board' pSpot & length
        nbCardDrawSkills =
          Board.toPlayerHoleyInPlace board pSpot
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
                & map (\case ItemCard _ ItemObject {item = i} | item == i -> Just i; _ -> Nothing)
                & catMaybes
         in found `shouldBe` [item]
    prop "maps all Neutral values" $
      \n ->
        let found =
              SharedModel.getCards shared
                & map (\case NeutralCard _ NeutralObject {neutral = n'} | n == n' -> Just n'; _ -> Nothing)
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
      _ <- newActorAt "a2" skeleton a1x a1y
      wait 1
    expectedScene = do
      _ <- newActorAt "a1" skeleton 1 2
      _ <- newActorAt "a2" skeleton 1 2
      wait 1

testAIPlace shared =
  describe "AI.placeCards" $ do
    prop "placeCards returns events whose spots differ" $
      \board turn -> allDiff $ AI.placeCards difficulty shared board turn
    prop "placeCards returns events whose card is valid" $
      \board pSpot -> AI.placeCards difficulty shared board pSpot `shouldSatisfy` (goodCards board pSpot)
    prop "placeCards returns events playing cards of the player whose turn it is" $
      \board pSpot -> AI.placeCards difficulty shared board pSpot `shouldSatisfy` playerIs pSpot
    prop "placeCards return events that commute (modulo Discipline)" $
      \(Pretty board) (Pretty turn) ->
        let events = AI.placeCards difficulty shared board turn & filter (not . hasDiscipline)
         in (length events >= 2)
              ==> forAll (Test.QuickCheck.elements (permutations events))
              $ \events' ->
                Pretty (ignoreErrMsg (Game.playAll shared board events)) `shouldBe` Pretty (ignoreErrMsg (Game.playAll shared board events'))
  where
    difficulty = AI.Easy
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
        & Card.unlift
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
        handSize = List.length $ Board.toHand board pSpot
    goodCards (board :: Board 'Core) pSpot (Game.Place' _ _ id : tl) =
      if id `elem` hand
        then goodCards board pSpot tl
        else traceShow ("Wrong ID: " ++ show id ++ "It does not belong to the hand: " ++ show hand) False
      where
        hand = Board.toHand board pSpot
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
        let score = flip boardPlayerScore pSpot
         in let (initialScore, events) = (score board, AI.play AI.Easy shared board pSpot)
             in let nextScore = Game.playAll shared board events & takeBoard <&> score
                 in monotonic initialScore nextScore
  where
    takeBoard (Left _) = Nothing
    takeBoard (Right (Game.Result _ b _ _)) = Just b
    monotonic _ Nothing = True -- Nothing to test
    monotonic i (Just j) = j <= i -- Better score is smaller score

testRewards =
  describe "Rewards work as expected" $ do
    it "initialisation" $ do
      Campaign.decks [] Campaign.Level0 Human == [[]]
    prop "one card is received at every reward" $ do
      \(level, team) ->
        Campaign.decks [] level team
          `shouldAllSatisfy` (\deck -> natLength deck == Campaign.nbRewards level)

testFear shared =
  describe "Fear works as expected" $ do
    it "fear triggers when expected" $
      (Board.toPart board'' PlayerBot & Board.inPlace) `shouldSatisfy` Map.null
    it "fear kills go to discarded stack" $
      (Board.toDiscarded board'' PlayerBot `shouldBe` [IDC fearTarget []])
    it "fear does not trigger when expected" $
      (Board.toPart boardBis'' PlayerBot & Board.inPlace) `shouldNotSatisfy` Map.null
    it "fear skill is consumed when it triggers" $
      Board.toInPlaceCreature board'' PlayerTop Bottom `shouldSatisfyJust` hasConsumedFear
  where
    teams = Teams Undead Human
    causingFear = CreatureID Skeleton Undead
    fearTarget = CreatureID Archer Human
    board = Board.small shared teams causingFear [] PlayerTop Bottom
    affectedByFear =
      SharedModel.idToCreature shared fearTarget []
        & fromJust
        & Card.unlift
    board' =
      Board.setCreature
        board
        PlayerBot
        (bottomSpotOfTopVisual Top)
        (affectedByFear & (\Creature {..} -> Creature {hp = 1, ..}))
    board'' = Game.play shared board' (Game.ApplyFearNTerror PlayerTop) & extract
    extract (Left _) = error "Test failure"
    extract (Right (Game.Result _ b _ _)) = b
    boardBis' =
      Board.setCreature
        board
        PlayerBot
        (bottomSpotOfTopVisual Top)
        affectedByFear
    boardBis'' = Game.play shared boardBis' (Game.ApplyFearNTerror PlayerTop) & extract
    hasConsumedFear :: Creature 'Core -> Bool
    hasConsumedFear Creature {skills} = go skills
      where
        go [] = False
        go (Fear' False : _) = True -- Fear unavailable: it has been consumed
        go (_ : tail) = go tail

testFearNTerror shared =
  describe "Fear and terror" $ do
    prop "Creature causing fear is immune to fear" $
      causingFear `shouldAllSatisfy` (not . Total.affectedByFear False)
    it "Creature causing terror is immune to fear" $
      causingTerror `shouldAllSatisfy` (not . Total.affectedByFear False)
    it "Creature causing terror is immune to terror" $
      causingTerror `shouldAllSatisfy` (not . Total.affectedByTerror False)
    it "Creature affected by fear is affected by terror" $
      causingFear `shouldAllSatisfy` Total.affectedByTerror False
  where
    creatures =
      SharedModel.getCards shared
        & mapMaybe (\case CreatureCard _ c -> Just c; _ -> Nothing)
        & map Card.unlift
    causingTerror = creatures & filter Total.causesTerror
    causingFear = creatures & filter Total.causesFear

testMana shared =
  describe "AI" $ do
    prop "only play cards for which enough mana is available" $
      \(board, pSpot) ->
        let manaAvailable = Board.toPart board pSpot & Board.mana
         in AI.play Easy shared board pSpot `shouldAllSatisfy` manaCostGeq board manaAvailable
  where
    -- manaCostGeq returns True if 'avail' is greater or equal to the
    -- mana cost of 'card'.
    manaCostGeq :: Board 'Core -> Nat -> Game.Event -> Bool
    manaCostGeq board avail =
      \case
        Game.ApplyFearNTerror {} -> True
        Game.Attack {} -> True
        Game.FillTheFrontline {} -> True
        Game.NoPlayEvent -> True
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
        go pSpot HandIndex {unHandIndex = i} = do
          id :: Card.ID <- card
          card :: Card 'UI <-
            SharedModel.identToCard shared id
              & (\case Nothing -> Left "card not found"; Just x -> Right x)
          return $ (Card.toCommon card & Card.mana) <= avail
          where
            card :: Either Text Card.ID =
              Board.lookupHand (Board.toHand board pSpot) i & runExcept

testPlague shared =
  describe "Plague" $ do
    prop "Plague kills creatures with hp <= 1" $
      \(teams :: Teams Team, cids :: [(CreatureID, [Item])]) ->
        applyPlague (setHp 1) teams cids `shouldSatisfy` Map.null
    prop "Plague doesn't kill creatures with hp > 1" $
      \(teams :: Teams Team, cids :: [(CreatureID, [Item])]) ->
        (applyPlague (setHp 2) teams cids & Map.size) `shouldBe` min 6 (length cids)
  where
    addAllToInPlace b pSpot cids = foldr (\pair b -> addToInPlace b pSpot pair) b cids
    addToInPlace (b :: Board 'Core) pSpot (cid, items) =
      case firstEmpty of
        Nothing -> b
        Just cSpot ->
          let creature = SharedModel.idToCreature shared cid items & fromJust & Card.unlift
           in Board.setInPlace b pSpot (Board.toInPlace b pSpot & Map.insert cSpot creature)
      where
        firstEmpty =
          Board.toPlayerHoleyInPlace b pSpot
            & filter (\(_, m) -> isNothing m)
            & listToMaybe
            <&> fst
    mkBoard teams pSpot cids = addAllToInPlace (Board.empty teams) pSpot cids
    mapInPlace f pSpot (board :: Board 'Core) =
      Board.toInPlace board pSpot
        & Map.map f
        & Board.setInPlace board pSpot
    setHp i c = c {hp = i}
    applyPlague f teams cids =
      Game.applyPlague board pSpot & flip Board.toInPlace pSpot
      where
        pSpot = PlayerTop
        board = mkBoard teams pSpot cids & mapInPlace f pSpot

mkCreature :: SharedModel -> CreatureKind -> Team -> Bool -> Creature 'Core
mkCreature shared kind team transient =
  SharedModel.idToCreature shared (CreatureID kind team) []
    & fromJust
    & Card.unlift
    & (\c -> c {transient})
  where
    fromJust Nothing = error $ "mkCreature: (" ++ show kind ++ "," ++ show team ++ ") not found"
    fromJust (Just x) = x

testFillTheFrontline shared =
  describe "Fill the frontline" $ do
    it "Fill the frontline applies only to Ranged creatures" $ do
      board' `shouldSatisfy` pred
  where
    (team, pSpot) = (Human, PlayerTop)
    mkCreature' kind team = mkCreature shared kind team False
    board =
      Board.empty (Teams team team)
        & (\b -> Board.setCreature b pSpot Top $ mkCreature' Archer team)
        & (\b -> Board.setCreature b pSpot TopLeft $ mkCreature' Spearman team)
    board' = Game.play shared board $ Game.FillTheFrontline pSpot
    pred (Left errMsg) = traceShow errMsg False
    pred (Right (Game.Result _ board'' _ _)) =
      Board.toInPlaceCreature board'' pSpot Top == Nothing -- Archer moved
        && (Board.toInPlaceCreature board'' pSpot Bottom ~= Archer) -- to frontline spot
        && (Board.toInPlaceCreature board'' pSpot TopLeft ~= Spearman) -- Spearman stayed in position
    (~=) Nothing _ = False
    (~=) (Just Creature {creatureId = CreatureID {creatureKind = actual}}) expected = actual == expected

testBreathIce :: SharedModel -> SpecWith ()
testBreathIce shared =
  describe "Breath ice" $ do
    it "Breath ice creature hits two creatures in a row when in the frontline" $ do
      board' Bottom `shouldSatisfy` pred
    it "Breath ice creature does nothing when in the backline" $ do
      board' Top `shouldSatisfy` pred'
  where
    (team, pSpot, otherpSpot) = (Undead, PlayerTop, otherPlayerSpot pSpot)
    mkCreature' kind team = mkCreature shared kind team False
    (dummy1, dummy2) = (Archer, Skeleton)
    mkBoard specterSpot =
      Board.empty (Teams team team)
        & (\b -> Board.setCreature b pSpot specterSpot $ mkCreature' Specter team)
        & (\b -> Board.setCreature b otherpSpot Top $ mkCreature' dummy1 team)
        & (\b -> Board.setCreature b otherpSpot Bottom $ mkCreature' dummy2 team)
    board' specterSpot =
      Game.play shared (mkBoard specterSpot) $ Game.Attack pSpot specterSpot False False
    pred (Left errMsg) = traceShow errMsg False
    pred (Right (Game.Result _ board'' _ _)) =
      Board.toInPlace board'' otherpSpot == mempty -- Both dummies killed
    pred' (Left errMsg) = traceShow errMsg False
    pred' (Right (Game.Result _ board'' _ _)) =
      Board.toInPlaceCreature board'' otherpSpot Top ~= dummy1 -- dummy1 stayed alive
        && (Board.toInPlaceCreature board'' otherpSpot Bottom ~= dummy2) -- dummy2 stayed alive
    (~=) Nothing _ = False
    (~=) (Just Creature {creatureId = CreatureID {creatureKind = actual}}) expected = actual == expected

testTransient shared =
  describe "Transient" $ do
    it "Transient creatures don't go to any stack when killed" $ do
      board' `shouldSatisfy` pred
  where
    botCardSpot = bottomSpotOfTopVisual Top
    board =
      Board.empty (Teams Undead Human)
        & (\b -> Board.setCreature b PlayerTop Bottom $ mkCreature shared Skeleton Undead True)
        & (\b -> Board.setCreature b PlayerBot botCardSpot $ mkCreature shared Vampire Undead True)
    board' = Game.play shared board $ Game.Attack PlayerBot botCardSpot False False
    pred (Left errMsg) = traceShow errMsg False
    pred (Right (Game.Result _ board'' _ _)) =
      Board.toInPlaceCreature board'' PlayerTop Bottom == Nothing -- Skeleton was killed
        && (map (Board.toDiscarded board'') allPlayersSpots & all null) -- Discarded stack is empty

testItemsAI shared =
  describe "AI" $ do
    it "Sword of Might is put on most potent in place creature" $
      play (board1 (mkCreature' Archer Undead) (mkCreature' Vampire Undead) SwordOfMight)
        `shouldSatisfy` ( \case
                            Left errMsg -> traceShow errMsg False
                            Right (Game.Result _ board' _ _) -> hasItem board' pSpot Bottom SwordOfMight
                        )
  where
    pSpot = PlayerTop
    board1 id1 id2 item =
      Board.empty teams
        & setCreature pSpot TopLeft id1
        & setCreature pSpot Bottom id2
        & (\b -> Board.addToHand b pSpot (IDI item))
    teams = Teams Undead Undead
    mkCreature' kind team = mkCreature shared kind team False
    setCreature pSpot cSpot c board =
      Board.setCreature board pSpot cSpot c
    play board =
      Game.playAll shared board $ AI.play AI.Easy shared board pSpot
    hasItem board pSpot cSpot item =
      case Board.toInPlaceCreature board pSpot cSpot of
        Nothing -> False
        Just Creature {items} -> item `elem` items

testApplyDifficulty stdgen =
  describe "applyDifficulty" $ do
    prop "Returned lists are permutations of the input list" $
      \(difficulty, SmallList (l :: [Int])) ->
        let perms = permutations l
         in AI.applyDifficulty difficulty stdgen l
              `shouldAllSatisfy` (`elem` perms)

main :: IO ()
main = hspec $ do
  let eitherCardsNTiles = loadJson
  let (cards, _, _) = eitherCardsNTiles ^?! _Right
  let allDecks = getAllDecks cards
  let allCreatures = mapMaybe cardToCreature $ concat allDecks
  describe "initial state is correct" $ do
    it "cards can be loaded from json" $
      is _Right eitherCardsNTiles -- should be the first test, others depend on it
    xit "all decks are initially of the same size (modulo items)" $
      let itemLessDecks = map (filter (\case ItemCard {} -> False; _ -> True)) allDecks
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
            Board.toHoleyInPlace (testAIRanged shared Turn.initial)
              & filter (\(_, _, maybeCreature) -> isJust maybeCreature)
       in all (\(_, cSpot, _) -> inTheBack cSpot) occupiedSpots
            && not (null occupiedSpots)
  -- From fast tests to slow tests (to maximize failing early)
  testRewards
  testFear shared
  testFearNTerror shared
  testItemsAI shared
  testPlague shared
  testFillTheFrontline shared
  testTransient shared
  testPlayFraming shared
  testDrawCards shared
  testShared shared
  testAIPlace shared
  testInPlaceEffectsMonoid
  testApplyDifficulty $ SharedModel.getStdGen shared
  testNoPlayEventNeutral shared
  testPlayScoreMonotonic shared
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
