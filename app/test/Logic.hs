{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- |
-- This module tests the game logic
-- |
module Logic (disturber, main, mkCreature, testPandemonium) where

-- This module should not import 'AI'. Tests of the AI are in 'Main'.

import Board
import qualified BoardInstances
import Card
import Constants
import Control.Lens hiding (at, (+=))
import Damage (Damage, (+^), (-^))
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace (traceShow)
import qualified Game
import Generators ()
import Pretty
import SharedModel (SharedModel, idToCreature)
import qualified SharedModel
import qualified Skill
import Spots
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import TestLib (shouldAllSatisfy, shouldSatisfyJust)
import qualified Total
import Turn

-- 'disturbingItem i' returns 'False' if playing 'i' only changes
-- the 'Spots.Card' it is applied on.
disturbingItem = \case
  Crown -> False
  CrushingMace -> False
  FlailOfTheDamned -> False
  SkBanner -> True
  SpikyMace -> False
  SwordOfMight -> False

-- 'disturber' identifies cards which, when played, affect other spots on the board
disturber :: SharedModel -> Card.ID -> Bool
disturber shared (IDC id items) =
  SharedModel.idToCreature shared id items
    & fromJust
    & Card.unlift
    & ( orf
          [ \Creature {skills} -> Skill.Squire `elem` skills,
            \Creature {items} -> any disturbingItem items,
            Total.isDisciplined
          ]
      )
  where
    orf :: [a -> Bool] -> a -> Bool
    orf [] = const False
    orf (f : fs) = \x -> (f x || orf fs x)
disturber _ (IDI item) = disturbingItem item
disturber _ (IDN neutral) =
  case neutral of
    Health -> False
    InfernalHaste -> True
    Life -> False
    Pandemonium -> True
    Plague -> True
    StrengthPot -> False

-- Tests that playing a creature only affects the target spot. Some
-- cards are omitted, see 'disturber.
testPlayFraming :: SharedModel -> SpecWith ()
testPlayFraming shared =
  describe
    "Playing some cards doesn't change"
    $ prop "other spots of the board when placing non-disturbing creature" $
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
          if disturber shared id
            then pickCardSpot (Board.setHand board pSpot hand') (i + 1) pSpot -- try next card
            else case targetType id of
              PlayerTargetType -> Nothing
              CardTargetType ctk ->
                Board.toPlayerCardSpots board pSpot ctk
                  & listToMaybe
                  <&> (i,)
    relation _ _ _ (Left _) = True
    relation board pSpot cSpot (Right (Game.PolyResult _ board' _ _)) = boardEq board pSpot [cSpot] board'
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
            & filter (\case Skill.DrawCard b -> b; _ -> False)
            & length

testNoPlayEventNeutral shared =
  describe "Game.NoPlayEvent is neutral w.r.t Game.playAll" $
    prop "Game.playAll $ [NoPlayEvent] ++ es == Game.playAll $ es ++ [NoPlayEvent]" $
      \(board, events) ->
        let left = [Game.NoPlayEvent] ++ events
         in let right = events ++ [Game.NoPlayEvent]
             in Game.playAll shared board left `shouldBe` Game.playAll shared board right

testFear :: SharedModel -> SpecWith ()
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
    extract (Right (Game.PolyResult _ b _ _)) = b
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
        go (Skill.Fear False : _) = True -- Fear unavailable: it has been consumed
        go (_ : tail) = go tail

testFearNTerror :: SharedModel -> SpecWith ()
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

-- | 'mkCreature shared kind t transient' creates a creature with the
-- given kind and team. The 'Bool' is whether the creature should be transient.
-- or not.
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
    pred (Right (Game.PolyResult _ board'' _ _)) =
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
    pred (Right (Game.PolyResult _ board'' _ _)) =
      Board.toInPlace board'' otherpSpot == mempty -- Both dummies killed
    pred' (Left errMsg) = traceShow errMsg False
    pred' (Right (Game.PolyResult _ board'' _ _)) =
      Board.toInPlaceCreature board'' otherpSpot Top ~= dummy1 -- dummy1 stayed alive
        && (Board.toInPlaceCreature board'' otherpSpot Bottom ~= dummy2) -- dummy2 stayed alive
    (~=) Nothing _ = False
    (~=) (Just Creature {creatureId = CreatureID {creatureKind = actual}}) expected = actual == expected

testChurch :: SharedModel -> SpecWith ()
testChurch shared =
  describe "Church" $ do
    prop "Church effect is as expected" $
      \(Pretty board, pSpot) ->
        Game.play shared board (Game.ApplyChurch pSpot) `shouldSatisfy` (pred board pSpot)
  where
    pred _ _ (Left errMsg) = traceShow errMsg False
    pred board pSpot (Right (Game.PolyResult _shared' board' _ _anim)) =
      Board.toPart board otherSpot == Board.toPart board' otherSpot -- Other spot is unchanged
        && all
          (\cSpot -> (toCreature board cSpot) ~= (toCreature board' cSpot))
          Spots.allCards
      where
        otherSpot = otherPlayerSpot pSpot
        toCreature (b :: Board 'Core) cSpot =
          Board.toInPlace b pSpot & (Map.!? cSpot)
        (~=) before after =
          case (before, after) of
            (Just c1@Creature {attack = a1, hp = hp1}, Just c2@Creature {attack = a2, hp = hp2}) ->
              if (c1 == c2) || (a1 == a2 -^ 1) || (hp1 == hp2 - 1)
                then True
                else traceShow c1 (traceShow c2 False)
            (Nothing, Nothing) -> True
            _ -> False

testKing :: SharedModel -> SpecWith ()
testKing shared =
  describe "King" $ do
    prop "King effect is as expected" $
      \(Pretty board, pSpot) ->
        Game.play shared board (Game.ApplyKing pSpot) `shouldSatisfy` (pred board pSpot)
  where
    pred _ _ (Left errMsg) = traceShow errMsg False
    pred board pSpot (Right (Game.PolyResult _shared' board' _ _anim)) =
      Board.toPart board otherSpot == Board.toPart board' otherSpot -- Other spot is unchanged
        && all
          (\cSpot -> (toCreature board cSpot) ~= (toCreature board' cSpot))
          Spots.allCards
      where
        otherSpot = otherPlayerSpot pSpot
        toCreature (b :: Board 'Core) cSpot =
          Board.toInPlace b pSpot & (Map.!? cSpot)
        (~=) before after =
          case (before, after) of
            (Just c1@Creature {attack = a1, hp = hp1, skills = skills1}, Just c2@Creature {attack = a2, hp = hp2, skills = skills2}) ->
              if (c1 == c2) || ((a1 < a2) && (hp1 < hp2) && skills1 == skills2 && Skill.Knight `elem` skills1)
                then True
                else traceShow c1 (traceShow c2 False)
            (Nothing, Nothing) -> True
            _ -> False

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
    pred (Right (Game.PolyResult _ board'' _ _)) =
      Board.toInPlaceCreature board'' PlayerTop Bottom == Nothing -- Skeleton was killed
        && (map (Board.toDiscarded board'') Spots.allPlayers & all null) -- Discarded stack is empty

testTeamDeck shared =
  describe "Card.rawTeamDeck" $ do
    prop "doesn't return None" $ do
      \team ->
        rawTeamDeck (SharedModel.getCards shared) team `shouldAllSatisfy` isJust

testCharge shared =
  describe "Skill.Charge" $ do
    it "does NOT trigger when it should not" $ do
      attack board pSpot BottomLeft
        `shouldBe` (Card.attack $ mkCreature' Card.Knight)
    it "triggers when it should" $ do
      attack board' pSpot BottomLeft
        `shouldBe` (Card.attack $ mkCreature' Card.Knight) +^ Constants.chargeAmount
  where
    board =
      Board.empty (Teams ZKnights Undead)
        & (\b -> Board.setCreature b pSpot BottomLeft knight)
        & (\b -> Board.setCreature b pSpot BottomRight knight)
    board' = Board.setCreature board pSpot Bottom knight
    pSpot = PlayerTop
    team = ZKnights
    mkCreature' k = mkCreature shared k team False
    knight :: Creature 'Core = mkCreature' Card.Knight
    attack b pSpot cSpot =
      Total.attack
        (Just $ Total.mkPlace b pSpot cSpot)
        (Board.toInPlaceCreature b pSpot cSpot & fromJust)

testSquire shared =
  describe "Skill.Squire" $ do
    it "does NOT trigger when it should not" $ do
      attack board pSpot Bottom `shouldBe` (Card.attack $ mkCreature' Card.Knight)
      attack board' pSpot Top `shouldBe` (Card.attack $ mkCreature' Card.Knight)
    it "triggers when it should" $ do
      attack board'' pSpot Bottom
        `shouldBe` (Card.attack $ mkCreature' Card.Knight) +^ Constants.squireAttackBonus
  where
    (team, pSpot) = (ZKnights, PlayerTop)
    board =
      Board.empty (Teams ZKnights Undead)
        & (\b -> Board.setCreature b pSpot Bottom knight) -- Knight alone
    board' =
      Board.empty (Teams ZKnights Undead)
        & (\b -> Board.setCreature b pSpot Bottom squire) -- Squire in front line
        & (\b -> Board.setCreature b pSpot Top knight) -- Knight in back line
    board'' = Board.setCreature board pSpot Top squire -- Add squire
    mkCreature' k = mkCreature shared k team False
    knight :: Creature 'Core = mkCreature' Card.Knight
    squire :: Creature 'Core = mkCreature' Card.Squire
    attack b pSpot cSpot =
      Total.attack
        (Just $ Total.mkPlace b pSpot cSpot)
        (Board.toInPlaceCreature b pSpot cSpot & fromJust & checkk)
    checkk (c@Creature {skills} :: Creature 'Core) | Skill.Knight `elem` skills = c
    checkk _ = error "Expected a knight"

testVeteran shared =
  describe "Skill.Veteran" $ do
    it "is a skill of the ZKnights veteran" $ do
      Skill.Veteran `elem` (Card.skills veteran)
    it "protects against fear" $ do
      Game.play shared board' (Game.ApplyFearNTerror otherSpot)
        `shouldSatisfy` (match board')
    it "protects against terror" $ do
      Game.play shared board'' (Game.ApplyFearNTerror otherSpot)
        `shouldSatisfy` (match board'')
  where
    (team, pSpot, otherSpot) = (ZKnights, PlayerTop, otherPlayerSpot pSpot)
    board =
      Board.empty (Teams team Undead)
        & (\b -> Board.setCreature b pSpot Bottom (veteran {hp = 1})) -- Captain alone
    board' = Board.setCreature board otherSpot (bottomSpotOfTopVisual Top) skeleton
    board'' = Board.setCreature board otherSpot (bottomSpotOfTopVisual Top) vampire
    veteran :: Creature 'Core = mkCreature shared Card.Veteran team False
    skeleton :: Creature 'Core = mkCreature shared Card.Skeleton Undead False
    vampire :: Creature 'Core = mkCreature shared Card.Vampire Undead False
    match :: (Board 'Core) -> (Either Text Game.Result) -> Bool
    match _ (Left errMsg) = traceShow errMsg False
    match expected (Right (Game.PolyResult _ b _ _)) = b == expected

testZealot shared =
  describe "Skill.Zealot" $ do
    it "is a skill of the ZKnights captain" $ do
      Skill.Zealot `elem` (Card.skills captain)
    it "protects against fear" $ do
      Game.play shared board' (Game.ApplyFearNTerror otherSpot)
        `shouldSatisfy` (match board')
    it "doesn't protect against terror" $ do
      Game.play shared board'' (Game.ApplyFearNTerror otherSpot)
        `shouldSatisfy` zPartIsEmpty
  where
    (team, pSpot, otherSpot) = (ZKnights, PlayerTop, otherPlayerSpot pSpot)
    board =
      Board.empty (Teams team Undead)
        & (\b -> Board.setCreature b pSpot Bottom (captain {hp = 1})) -- Captain alone
    board' = Board.setCreature board otherSpot (bottomSpotOfTopVisual Top) skeleton
    board'' = Board.setCreature board otherSpot (bottomSpotOfTopVisual Top) vampire
    captain :: Creature 'Core = mkCreature shared Card.Captain team False
    skeleton :: Creature 'Core = mkCreature shared Card.Skeleton Undead False
    vampire :: Creature 'Core = mkCreature shared Card.Vampire Undead False
    match :: (Board 'Core) -> (Either Text Game.Result) -> Bool
    match _ (Left errMsg) = traceShow errMsg False
    match expected (Right (Game.PolyResult _ b _ _)) = b == expected
    zPartIsEmpty (Left errMsg) = traceShow errMsg False
    zPartIsEmpty (Right (Game.PolyResult _ b _ _)) =
      Board.toInPlace b pSpot == mempty

testStrengthPot shared =
  describe "Potion of Strength" $ do
    it "sanity check" $ do
      Card.attack beholder == Card.attack beholderUI
    it "increases attack" $ do
      Board.toInPlaceCreature board' pSpot cSpot
        `shouldSatisfyJust` (\c -> Total.attack Nothing c > Card.attack beholder)
    it "is reset by new turn" $ do
      Board.toInPlaceCreature board'' pSpot cSpot
        `shouldSatisfyJust` (\c -> Total.attack Nothing c == Card.attack beholder)
  where
    (team, pSpot, cSpot, strength) = (Evil, PlayerTop, Bottom, IDN StrengthPot)
    board :: Board 'Core =
      Board.small shared (Teams team team) (CreatureID ckind team) [] pSpot cSpot
        & (\b -> Board.addToHand b pSpot strength)
    board' :: Board 'Core =
      Game.play shared board (Game.Place' pSpot (Game.CardTarget pSpot cSpot) strength)
        & (\case Left errMsg -> error $ Text.unpack errMsg; Right b' -> b')
        & (\(Game.PolyResult _ b _ _) -> b)
    board'' :: Board 'Core = BoardInstances.boardStart board' pSpot
    ckind = Card.Beholder
    beholder :: Creature 'Core = mkCreature shared ckind team False
    beholderUI :: Creature 'UI =
      SharedModel.idToCreature shared (CreatureID ckind team) []
        & fromJust

testPandemonium shared =
  describe "Pandemonium" $ do
    it "shuffles the concerned part" $ do
      apply 0 shared board
  where
    (team, pSpot, cSpot, pandemonium) = (Evil, PlayerTop, Bottom, IDN Pandemonium)
    board :: Board 'Core =
      Board.small shared (Teams team team) (CreatureID Card.Beholder team) [] pSpot cSpot
    apply (count :: Int) sh b
      | count >= 64 = False -- Giving up
      | (Board.toInPlace b' pSpot & Map.keys) /= [Bottom] = True
      | otherwise = apply (count + 1) shared' b'
      where
        (shared', b') =
          Game.play sh (prepare b) (Game.Place' pSpot (Game.PlayerTarget pSpot) pandemonium)
            & (\case Left errMsg -> error $ Text.unpack errMsg; Right c -> c)
            & (\(Game.PolyResult s c _ _) -> (s, c))
        prepare b =
          Board.addToHand b pSpot pandemonium -- So that card is here
            & Board.mapMana pSpot ((+) 5) -- And mana is available to play it

testStatChange =
  describe "StatChange is a well behaved Monoid" $ do
    prop "mempty <> change == change" $
      \(change :: Game.StatChange) ->
        mempty <> change `shouldBe` change
    prop "change <> mempty == change" $
      \(change :: Game.StatChange) ->
        change <> mempty `shouldBe` change
    prop "change <> change' == change' <> change" $
      \(c1 :: Game.StatChange, c2) ->
        c1 <> c2 `shouldBe` c1 <> c2

testDamageMonoid =
  describe "Damage is a well behaved Monoid" $ do
    prop "mempty <> d == d" $
      \(d :: Damage) ->
        mempty <> d `shouldBe` d
    prop "d <> mempty == d" $
      \(d :: Damage) ->
        d <> mempty `shouldBe` d
    prop "d <> d' == d' <> d" $
      \(d :: Damage, d') ->
        d <> d' `shouldBe` d' <> d

main :: SharedModel -> SpecWith ()
main shared = do
  -- Tests are ordered from the fastest to the slowest
  -- Unit tests
  testFear shared
  testFillTheFrontline shared
  testTransient shared
  testBreathIce shared
  testCharge shared
  testPandemonium shared
  testSquire shared
  testStrengthPot shared
  testVeteran shared
  testZealot shared
  -- PBT tests
  testDamageMonoid
  testStatChange
  testTeamDeck shared
  testFearNTerror shared
  testChurch shared
  testKing shared
  testPlague shared
  testPlayFraming shared
  testDrawCards shared
  testNoPlayEventNeutral shared
