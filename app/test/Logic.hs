{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- |
-- This module tests the game logic
-- |
module Logic (main, mkCreature) where

-- This module should not import 'AI'. Tests of the AI are in 'Main'.

import Board
import Card
import Constants
import Control.Lens hiding (at, (+=))
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace (traceShow)
import qualified Game
import Generators ()
import Pretty
import SharedModel (SharedModel, idToCreature)
import qualified SharedModel
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import TestLib (shouldAllSatisfy, shouldSatisfyJust)
import qualified Total
import Turn

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

main :: SharedModel -> SpecWith ()
main shared = do
  -- Unit tests
  testFear shared
  testFillTheFrontline shared
  testTransient shared
  testBreathIce shared
  -- PBT tests
  testFearNTerror shared
  testPlague shared
  testPlayFraming shared
  testDrawCards shared
  testNoPlayEventNeutral shared
