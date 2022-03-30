{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
module Logic (main, mkCreature, testRampage) where

-- This module should not import 'AI'. Tests of the AI are in 'Main'.

import qualified Board
import qualified BoardInstances
import Card
import Constants
import Control.Lens hiding (at, (+=))
import Damage (Damage, (+^), (-^))
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as Text
import Debug.Trace (traceShow)
import qualified Game hiding (Playable (..))
import Generators ()
import qualified Mana
import qualified Move
import Pretty
import qualified Shared
import qualified Skill
import Spots (Card (..), Player (..))
import qualified Spots
import Test.Hspec
import Test.Hspec.QuickCheck
import TestLib (shouldAllSatisfy, shouldSatisfyJust, shouldSatisfyRight)
import qualified Total
import Turn

testDrawCards :: Shared.Model -> SpecWith ()
testDrawCards shared =
  describe
    "Drawing cards"
    $ prop "draws the expected number" $
      \(Pretty board, Pretty turn) ->
        let pSpot = Turn.toPlayerSpot turn
         in let srcs = Game.cardsToDraw board pSpot True
             in Game.drawCards shared board pSpot srcs `shouldSatisfy` cond board pSpot
  where
    cond board pSpot (_, board', _) =
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
      \(board, events, turn) ->
        let play es = Game.playAll shared $ Game.mkPlayable board es turn
         in play ([Game.NoPlayEvent] ++ events)
              `shouldBe` play (events ++ [Game.NoPlayEvent])
  where

testFear :: Shared.Model -> SpecWith ()
testFear shared =
  describe "Fear works as expected" $ do
    it "fear triggers when expected" $
      (applyFear board' & toEither <&> (Board.inPlace . (flip Board.toPart otherPSpot)))
        `shouldSatisfyRight` Map.null
    it "fear kills go to discarded stack" $
      (applyFear board' & toEither <&> (flip Board.toDiscarded otherPSpot))
        `shouldBe` (Right [IDC fearTarget []])
    it "fear does not trigger when expected" $
      (applyFear board'' & toEither <&> (Board.inPlace . (flip Board.toPart otherPSpot)))
        `shouldSatisfyRight` (not . Map.null)
    it "fear skill is consumed when it triggers" $
      (applyFear board' & toEither <&> (\b -> Board.toInPlaceCreature b causingFearPSpot Bottom & fromJust))
        `shouldSatisfyRight` hasConsumedFear
  where
    teams = Board.Teams Undead Human
    (causingFear, fearTarget) = (CreatureID Skeleton Undead, CreatureID Archer Human)
    (causingFearPSpot, otherPSpot) = (PlayerTop, Spots.other causingFearPSpot)
    board = Board.small shared teams causingFear [] causingFearPSpot Bottom
    affectedByFear :: Creature 'Core =
      Shared.idToCreature shared fearTarget []
        & fromJust
        & Card.unlift
        & (\Creature {..} -> Creature {hp = 1, ..})
    board' = Board.setCreature otherPSpot (Board.bottomSpotOfTopVisual Top) affectedByFear board
    applyFear b =
      Game.play shared $
        Game.mkPlayable b (Game.ApplyFearNTerror causingFearPSpot) Turn.initial
    board'' = Board.setCreature otherPSpot (Board.bottomSpotOfTopVisual Bottom) affectedByFear board
    toEither (Left errMsg) = Left errMsg
    toEither (Right (Game.Result {board = b})) = Right b
    hasConsumedFear Creature {skills} = any ((==) (Skill.Fear False)) skills

testFearNTerror :: Shared.Model -> SpecWith ()
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
      Shared.getCards shared
        & mapMaybe (\case CreatureCard _ c -> Just c; _ -> Nothing)
        & map Card.unlift
    causingTerror = creatures & filter Total.causesTerror
    causingFear = creatures & filter Total.causesFear

testPlague shared =
  describe "Plague" $ do
    prop "Plague kills creatures with hp <= 1" $
      \(teams :: Board.Teams Team, cids :: [(CreatureID, [Item])]) ->
        applyPlague (setHp 1) teams cids `shouldSatisfy` Map.null
    prop "Plague doesn't kill creatures with hp > 1" $
      \(teams :: Board.Teams Team, cids :: [(CreatureID, [Item])]) ->
        (applyPlague (setHp 2) teams cids & Map.size) `shouldBe` min 6 (length cids)
  where
    addAllToInPlace b pSpot cids = foldr (\pair b -> addToInPlace b pSpot pair) b cids
    addToInPlace (b :: Board.T 'Core) pSpot (cid, items) =
      case firstEmpty of
        Nothing -> b
        Just cSpot ->
          let creature = Shared.idToCreature shared cid items & fromJust & Card.unlift
           in Board.setInPlace pSpot (Board.toInPlace b pSpot & Map.insert cSpot creature) b
      where
        firstEmpty =
          Board.toPlayerHoleyInPlace b pSpot
            & filter (\(_, m) -> isNothing m)
            & listToMaybe
            <&> fst
    mkBoard teams pSpot cids = addAllToInPlace (Board.empty teams) pSpot cids
    mapInPlace f pSpot (board :: Board.T 'Core) =
      Board.toInPlace board pSpot
        & Map.map f
        & (\x -> Board.setInPlace pSpot x board)
    setHp i c = c {hp = i}
    applyPlague f teams cids =
      Game.applyPlague board pSpot & flip Board.toInPlace pSpot
      where
        pSpot = PlayerTop
        board = mkBoard teams pSpot cids & mapInPlace f pSpot

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

-- | 'mkCreature shared kind t transient' creates a creature with the
-- given kind and team. The 'Bool' is whether the creature should be transient.
-- or not.
mkCreature :: Shared.Model -> CreatureKind -> Team -> Bool -> Creature 'Core
mkCreature shared kind team transient =
  Shared.idToCreature shared (CreatureID kind team) []
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
      Board.empty (Board.Teams team team)
        & Board.setCreature pSpot Top (mkCreature' Archer team)
        & Board.setCreature pSpot TopLeft (mkCreature' Spearman team)
    board' =
      Game.play shared $
        Game.mkPlayable board (Game.FillTheFrontline pSpot) Turn.initial
    pred (Left errMsg) = traceShow errMsg False
    pred (Right (Game.Result {board = board''})) =
      Board.toInPlaceCreature board'' pSpot Top == Nothing -- Archer moved
        && (Board.toInPlaceCreature board'' pSpot Bottom ~= Archer) -- to frontline spot
        && (Board.toInPlaceCreature board'' pSpot TopLeft ~= Spearman) -- Spearman stayed in position
    (~=) Nothing _ = False
    (~=) (Just Creature {creatureId = CreatureID {creatureKind = actual}}) expected = actual == expected

testBreathIce :: Shared.Model -> SpecWith ()
testBreathIce shared =
  describe "Breath ice" $ do
    it "Breath ice creature hits two creatures in a row when in the frontline" $ do
      board' Bottom `shouldSatisfyRight` pred
    it "Breath ice creature does nothing when in the backline" $ do
      board' Top `shouldSatisfyRight` pred'
  where
    (team, pSpot, otherpSpot) = (Undead, PlayerTop, Spots.other pSpot)
    mkCreature' kind team = mkCreature shared kind team False
    (dummy1, dummy2) = (Archer, Skeleton)
    mkBoard specterSpot =
      Board.empty (Board.Teams team team)
        & Board.setCreature pSpot specterSpot (mkCreature' Specter team)
        & Board.setCreature otherpSpot Top (mkCreature' dummy1 team)
        & Board.setCreature otherpSpot Bottom (mkCreature' dummy2 team)
    board' specterSpot =
      Game.play shared $
        Game.mkPlayable
          (mkBoard specterSpot)
          (Game.Attack pSpot specterSpot False False)
          Turn.initial
    pred (Game.Result {board = board''}) =
      Board.toInPlace board'' otherpSpot == mempty -- Both dummies killed
    pred' (Game.Result {board = board''}) =
      Board.toInPlaceCreature board'' otherpSpot Top ~= dummy1 -- dummy1 stayed alive
        && (Board.toInPlaceCreature board'' otherpSpot Bottom ~= dummy2) -- dummy2 stayed alive
    (~=) Nothing _ = False
    (~=) (Just Creature {creatureId = CreatureID {creatureKind = actual}}) expected = actual == expected

testChurch :: Shared.Model -> SpecWith ()
testChurch shared =
  describe "Church" $ do
    prop "Church effect is as expected" $
      \(Pretty board, pSpot, turn) ->
        Game.play shared (Game.mkPlayable board (Game.ApplyChurch pSpot) turn)
          `shouldSatisfyRight` (pred board pSpot)
  where
    pred board pSpot (Game.Result {board = board'}) =
      Board.toPart board otherSpot == Board.toPart board' otherSpot -- Other spot is unchanged
        && all
          (\cSpot -> (toCreature board cSpot) ~= (toCreature board' cSpot))
          Spots.allCards
      where
        otherSpot = Spots.other pSpot
        toCreature (b :: Board.T 'Core) cSpot =
          Board.toInPlace b pSpot & (Map.!? cSpot)
        (~=) before after =
          case (before, after) of
            (Just c1@Creature {attack = a1, hp = hp1}, Just c2@Creature {attack = a2, hp = hp2}) ->
              if (c1 == c2) || (a1 == a2 -^ 1) || (hp1 == hp2 - 1)
                then True
                else traceShow c1 (traceShow c2 False)
            (Nothing, Nothing) -> True
            _ -> False

testKing :: Shared.Model -> SpecWith ()
testKing shared =
  describe "King" $ do
    prop "King effect is as expected" $
      \(Pretty board, pSpot, turn) ->
        (Game.play shared (Game.mkPlayable board (Game.ApplyKing pSpot) turn))
          `shouldSatisfyRight` (pred board pSpot)
  where
    pred board pSpot (Game.Result {board = board'}) =
      Board.toPart board otherSpot == Board.toPart board' otherSpot -- Other spot is unchanged
        && all
          (\cSpot -> (toCreature board cSpot) ~= (toCreature board' cSpot))
          Spots.allCards
      where
        otherSpot = Spots.other pSpot
        toCreature (b :: Board.T 'Core) cSpot =
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
    botCardSpot = Board.bottomSpotOfTopVisual Top
    board =
      Board.empty (Board.Teams Undead Human)
        & Board.setCreature PlayerTop Bottom (mkCreature shared Skeleton Undead True)
        & Board.setCreature PlayerBot botCardSpot (mkCreature shared Vampire Undead True)
    board' =
      Game.play shared $
        Game.mkPlayable board (Game.Attack PlayerBot botCardSpot False False) Turn.initial
    pred (Left errMsg) = traceShow errMsg False
    pred (Right (Game.Result {board = board''})) =
      Board.toInPlaceCreature board'' PlayerTop Bottom == Nothing -- Skeleton was killed
        && (map (Board.toDiscarded board'') Spots.allPlayers & all null) -- Discarded stack is empty

testTeamDeck shared =
  describe "Card.rawTeamDeck" $ do
    prop "doesn't return None" $ do
      \team ->
        rawTeamDeck (Shared.getCards shared) team `shouldAllSatisfy` isJust

testCharge shared =
  describe "Skill.Charge" $ do
    it "does NOT trigger when it should not" $ do
      attack board pSpot BottomLeft
        `shouldBe` (Card.attack $ mkCreature' Card.Knight)
    it "triggers when it should" $ do
      attack (Board.setCreature pSpot Bottom knight board) pSpot BottomLeft
        `shouldBe` (Card.attack $ mkCreature' Card.Knight) +^ Constants.chargeAmount
  where
    board =
      Board.empty (Board.Teams ZKnights Undead)
        & Board.setCreature pSpot BottomLeft knight
        & Board.setCreature pSpot BottomRight knight
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
      Board.empty (Board.Teams ZKnights Undead)
        & Board.setCreature pSpot Bottom knight -- Knight alone
    board' =
      Board.empty (Board.Teams ZKnights Undead)
        & Board.setCreature pSpot Bottom squire -- Squire in front line
        & Board.setCreature pSpot Top knight -- Knight in back line
    board'' = Board.setCreature pSpot Top squire board -- Add squire
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
      (Game.play shared (Game.mkPlayable board' (Game.ApplyFearNTerror otherSpot) Turn.initial))
        `shouldSatisfyRight` (match board')
    it "protects against terror" $ do
      (Game.play shared (Game.mkPlayable board'' (Game.ApplyFearNTerror otherSpot) Turn.initial))
        `shouldSatisfyRight` (match board'')
  where
    (team, pSpot, otherSpot) = (ZKnights, PlayerTop, Spots.other pSpot)
    board =
      Board.empty (Board.Teams team Undead)
        & Board.setCreature pSpot Bottom (veteran {hp = 1}) -- Captain alone
    board' = Board.setCreature otherSpot (Board.bottomSpotOfTopVisual Top) skeleton board
    board'' = Board.setCreature otherSpot (Board.bottomSpotOfTopVisual Top) vampire board
    veteran :: Creature 'Core = mkCreature shared Card.Veteran team False
    skeleton :: Creature 'Core = mkCreature shared Card.Skeleton Undead False
    vampire :: Creature 'Core = mkCreature shared Card.Vampire Undead False
    match expected (Game.Result {board = b}) = b == expected

testZealot shared =
  describe "Skill.Zealot" $ do
    it "is a skill of the ZKnights captain" $ do
      Skill.Zealot `elem` (Card.skills captain)
    it "protects against fear" $ do
      Game.play shared (Game.mkPlayable board' (Game.ApplyFearNTerror otherSpot) Turn.initial)
        `shouldSatisfyRight` (match board')
    it "doesn't protect against terror" $ do
      Game.play shared (Game.mkPlayable board'' (Game.ApplyFearNTerror otherSpot) Turn.initial)
        `shouldSatisfyRight` zPartIsEmpty
  where
    (team, pSpot, otherSpot) = (ZKnights, PlayerTop, Spots.other pSpot)
    board =
      Board.empty (Board.Teams team Undead)
        & Board.setCreature pSpot Bottom (captain {hp = 1}) -- Captain alone
    board' = Board.setCreature otherSpot (Board.bottomSpotOfTopVisual Top) skeleton board
    board'' = Board.setCreature otherSpot (Board.bottomSpotOfTopVisual Top) vampire board
    captain :: Creature 'Core = mkCreature shared Card.Captain team False
    skeleton :: Creature 'Core = mkCreature shared Card.Skeleton Undead False
    vampire :: Creature 'Core = mkCreature shared Card.Vampire Undead False
    match expected (Game.Result {board = b}) = b == expected
    zPartIsEmpty (Game.Result {board = b}) = Board.toInPlace b pSpot == mempty

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
    board :: Board.T 'Core =
      Board.small shared (Board.Teams team team) (CreatureID ckind team) [] pSpot cSpot
        & (\b -> Board.addToHand b pSpot strength)
    board' :: Board.T 'Core =
      Game.play
        shared
        ( Game.mkPlayable
            board
            (Game.PEvent (Game.Place' pSpot (Game.CardTarget pSpot cSpot) strength))
            Turn.initial
        )
        & (\case Left errMsg -> error $ Text.unpack errMsg; Right b' -> b')
        & Game.board
    board'' :: Board.T 'Core = BoardInstances.boardStart board' pSpot
    ckind = Card.Beholder
    beholder :: Creature 'Core = mkCreature shared ckind team False
    beholderUI :: Creature 'UI =
      Shared.idToCreature shared (CreatureID ckind team) []
        & fromJust

testPandemonium shared =
  describe "Pandemonium" $ do
    it "shuffles the concerned part" $ do
      apply 0 shared board
  where
    (team, pSpot, cSpot, pandemonium) = (Evil, PlayerTop, Bottom, IDN Pandemonium)
    board :: Board.T 'Core =
      Board.small shared (Board.Teams team team) (CreatureID Card.Beholder team) [] pSpot cSpot
    apply (count :: Int) sh b
      | count >= 64 = False -- Giving up
      | (Board.toInPlace b' pSpot & Map.keys) /= [Bottom] = True
      | otherwise = apply (count + 1) shared' b'
      where
        (shared', b') =
          Game.play
            sh
            ( Game.mkPlayable
                (prepare b)
                (Game.PEvent (Game.Place' pSpot (Game.PlayerTarget pSpot) pandemonium))
                Turn.initial
            )
            & (\case Left errMsg -> error $ Text.unpack errMsg; Right c -> c)
            & (\(Game.Result {shared = s, board = c}) -> (s, c))
        prepare b =
          Board.addToHand b pSpot pandemonium -- So that card is here
            & Board.mapMana pSpot ((+) 5) -- And mana is available to play it

testAce shared =
  describe "Ace" $ do
    it "is a skill of the Beholder" $ do
      Card.has beholder (Skill.Ace :: Skill.State)
    it "can kill creatures anywhere in the enemy part" $ do
      Board.toInPlace (snd (attack 4 shared board)) enemyPSpot `shouldSatisfy` null
  where
    (team, pSpot, enemyPSpot, cSpot, ckind) =
      (Evil, PlayerTop, Spots.other pSpot, Top, Card.Beholder)
    beholder = mkCreature shared ckind team False
    enemy = mkCreature shared Card.Skeleton Undead False
    board :: Board.T 'Core =
      Board.small shared (Board.Teams team team) (creatureId beholder) [] pSpot cSpot
        & addEnemy Spots.Top
        & addEnemy Spots.Bottom
        & addEnemy Spots.TopLeft
        & addEnemy Spots.TopRight
    addEnemy cSpot = Board.setCreature enemyPSpot cSpot enemy
    attack (count :: Int) sh b =
      if count <= 0
        then (sh, b)
        else attack (count - 1) sh' b'
      where
        (sh', b') =
          Game.play sh (Game.mkPlayable b (Game.Attack pSpot cSpot False False) Turn.initial)
            & (\case Left errMsg -> error $ Text.unpack errMsg; Right c -> c)
            & (\(Game.Result {board = c, shared = s}) -> (s, c))

testPowerful shared =
  describe "Powerful" $ do
    it "is a skill of the Daemon" $ do
      Card.has daemon (Skill.Powerful :: Skill.State)
    it "works as expected" $ do
      (attack board <&> Board.toScore pSpot) `shouldSatisfyRight` ((<) 0)
  where
    (team, pSpot, enemyPSpot, cSpot, ckind) =
      (Evil, PlayerTop, Spots.other pSpot, Spots.Bottom, Card.Daemon)
    daemon = mkCreature shared ckind team False
    enemy = mkCreature shared Card.Skeleton Undead False
    board :: Board.T 'Core =
      Board.small shared (Board.Teams team team) (creatureId daemon) [] pSpot cSpot
        & Board.setCreature enemyPSpot cSpot enemy
    attack b =
      Game.play shared (Game.mkPlayable b (Game.Attack pSpot cSpot False False) Turn.initial)
        <&> Game.board

testRampage shared =
  describe "Rampage" $ do
    it "is a skill of the Bear" $ do
      Card.has bear (Skill.Rampage :: Skill.State)
    it "works as expected" $ do
      (attack board <&> flip Board.toInPlace enemyPSpot) `shouldSatisfyRight` null
  where
    (team, pSpot, enemyPSpot, cSpot, ckind) =
      (Sylvan, PlayerTop, Spots.other pSpot, Spots.Bottom, Card.Bear)
    bear = mkCreature shared ckind team False
    enemy = mkCreature shared Card.Skeleton Undead False
    board :: Board.T 'Core =
      Board.small shared (Board.Teams team team) (creatureId bear) [] pSpot cSpot
        & Board.setCreature enemyPSpot cSpot enemy
        & Board.setCreature enemyPSpot (Spots.switchLine cSpot) enemy
    attack b =
      Game.play shared (Game.mkPlayable b (Game.Attack pSpot cSpot False False) Turn.initial)
        <&> Game.board

testAxeOfRage shared =
  describe "Axe of Rage" $ do
    it "gives the powerful skill (1/2)" $ do
      not (Total.isPowerful sk)
    it "gives the powerful skill (2/2)" $ do
      Total.isPowerful (addItem Card.AxeOfRage sk)
  where
    sk = mkCreature shared Card.Skeleton Undead False
    addItem i (c@Creature {items} :: Creature 'Core) = c {items = items ++ [i]}

testScore shared =
  describe "Score" $ do
    it "is increased by fighter in front" $ do
      (attack (addFighter Bottom board) Bottom <&> Board.toScore pSpot)
        `shouldSatisfyRight` (\x -> x > 0)
    it "is not increased by fighter in the back" $ do
      (attack (addFighter Top board) Top <&> Board.toScore pSpot)
        `shouldSatisfyRight` ((==) 0)
  where
    (team, pSpot, ckind) = (Evil, PlayerTop, Card.Knight)
    fighter = mkCreature shared ckind team False
    board :: Board.T 'Core = Board.empty (Board.Teams team team)
    addFighter cSpot = Board.setCreature pSpot cSpot fighter
    attack b cSpot =
      Game.play shared (Game.mkPlayable b (Game.Attack pSpot cSpot False False) Turn.initial)
        <&> Game.board

testSupport shared =
  describe "Support" $ do
    it "is a skill of the Human spearman" $ do
      Card.has fighter (Skill.Support :: Skill.State)
    it "is not a skill of the Undead skeleton" $ do
      not $ Card.has victim (Skill.Support :: Skill.State)
    it "triggers when in the back line" $ do
      -- Victim must always be two cells away from fighter
      ( addFighter Top board & addVictim (Spots.bottomSpotOfTopVisual Top) & attack Top
          & flip Board.toInPlace victimPSpot
        )
        `shouldBe` mempty
    it "does not trigger when in front line" $ do
      ( addFighter Bottom board & addVictim (Spots.bottomSpotOfTopVisual Bottom) & attack Bottom
          & flip Board.toInPlace victimPSpot
          & Map.elems
        )
        `shouldBe` [victim]
  where
    (fTeam, vTeam) = (Human, Undead)
    (fighter, fighterPSpot) = (mkCreature shared Card.Spearman fTeam False, PlayerTop)
    (victim, victimPSpot) = (mkCreature shared Card.Skeleton vTeam False, Spots.other fighterPSpot)
    board :: Board.T 'Core = Board.empty (Board.Teams fTeam vTeam)
    addFighter cSpot = Board.setCreature fighterPSpot cSpot fighter
    addVictim cSpot = Board.setCreature victimPSpot cSpot victim
    attack cSpot b =
      Game.play shared (Game.mkPlayable b (Game.Attack fighterPSpot cSpot False False) Turn.initial)
        & (\case Left errMsg -> error $ Text.unpack errMsg; Right c -> c)
        & Game.board

testAssassins shared =
  describe "Assassin" $ do
    it "is a skill of the Evil assassin" $ do
      fighter `Card.has` (Skill.Assassin :: Skill.State)
    it "does not trigger when opponent is in back line" $ do
      let board' = addVictim (Spots.bottomSpotOfTopVisual Bottom) board
      applyAssassin fighterPSpot board' `shouldBe` board'
    it "triggers when opponents is in front line" $ do
      let board' = addVictim (Spots.bottomSpotOfTopVisual Top) board
      ( applyAssassin fighterPSpot board'
          & flip Board.toInPlace fighterPSpot
          & Map.partitionWithKey (\cSpot _ -> Spots.inFront cSpot)
          & Bifunctor.bimap Map.elems length
        )
        `shouldBe` ([fighter], 0)
  where
    (fTeam, vTeam) = (Evil, Undead)
    (fighter, fighterPSpot) = (mkCreature shared Card.Assassin fTeam False, PlayerTop)
    (victim, victimPSpot) = (mkCreature shared Card.Skeleton vTeam False, Spots.other fighterPSpot)
    board :: Board.T 'Core =
      Board.empty (Board.Teams fTeam vTeam)
        & Board.setCreature fighterPSpot Spots.Bottom fighter
    addVictim cSpot = Board.setCreature victimPSpot cSpot victim
    applyAssassin pSpot b =
      Game.play shared (Game.mkPlayable b (Game.ApplyAssassins pSpot) Turn.initial)
        & (\case Left errMsg -> error $ Text.unpack errMsg; Right c -> c)
        & Game.board

testPreEndTurnEventNextAttackSpot shared =
  describe "Executing pre EndTurn events doesn't change the next attack spot" $
    prop "prop" $
      \(Pretty board, pSpot, cSpot, turn) ->
        let nextAttackSpot b = Game.nextAttackSpot b pSpot cSpot
            events = Move.mkPreEndTurnEvents shared turn pSpot board
            board' =
              Game.playAll shared (Game.mkPlayable board events Turn.initial)
                & (\case Left errMsg -> error $ Text.unpack errMsg; Right x -> x)
                & Game.board
         in nextAttackSpot board `shouldBe` nextAttackSpot board'

testManaTurnOrd = do
  describe "<= Turn.Turn works as expected" $ do
    it "Turn.initial" $
      (Turn.initial <= Turn.initial) `shouldBe` True
    it "Turn.next" $ do
      (Turn.initial <= Turn.next Turn.initial) `shouldBe` True
      (Turn.next Turn.initial <= Turn.initial) `shouldBe` False
      (Turn.next (Turn.next Turn.initial) <= Turn.initial) `shouldBe` False
      (Turn.next Turn.initial <= Turn.next Turn.initial) `shouldBe` True
  describe "Mana.<=" $ do
    it "Mana" $ do
      ((Mana.<=) Turn.initial (Mana.Const 0) 0) `shouldBe` True
      ((Mana.<=) Turn.initial (Mana.Const 1) 1) `shouldBe` True
      ((Mana.<=) Turn.initial (Mana.Const 0) 1) `shouldBe` True
      ((Mana.<=) Turn.initial (Mana.Const 1) 0) `shouldBe` False

testManaRemainingTurns =
  describe "Mana.amount is inversely monotonic with Turn.Turn" $ do
    prop "prop" $
      \(m :: Mana.Mana, t1 :: Turn.Turn, t2 :: Turn.Turn) ->
        case compare t1 t2 of
          LT -> (Mana.amount t1 m >= Mana.amount t2 m) `shouldBe` True
          GT -> (Mana.amount t1 m <= Mana.amount t2 m) `shouldBe` True
          EQ -> pure ()

main :: Shared.Model -> SpecWith ()
main shared = do
  -- Unit tests
  testAce shared
  testAssassins shared
  testAxeOfRage shared
  testBreathIce shared
  testCharge shared
  testFear shared
  testFillTheFrontline shared
  testManaRemainingTurns
  testManaTurnOrd
  testTransient shared
  testPandemonium shared
  testPowerful shared
  testRampage shared
  testScore shared
  testSupport shared
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
  testDrawCards shared
  testNoPlayEventNeutral shared
  testPreEndTurnEventNextAttackSpot shared
