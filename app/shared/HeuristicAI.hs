{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- This module defines how the AI plays, using only an heuristic function
-- (contrary to 'MCTSAI').
-- 'applyDifficulty', 'boardScore', and 'playHand' are exported for tests
module HeuristicAI (applyDifficulty, boardPlayerScore, HeuristicAI.play, targets) where

import qualified Board
import BoardInstances ()
import Card
import Constants
import Control.Arrow ((>>>))
import Control.Exception
import Control.Monad.Except
import Damage (Damage (..))
import qualified Data.Bifunctor as Bifunctor
import Data.Either.Extra
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Debug.Trace (traceShow, traceShowId)
import qualified Game
import qualified Move
import Nat
import qualified Random
import qualified Shared
import qualified Skill
import Spots hiding (Card)
import qualified Spots
import System.Random (StdGen)
import System.Random.Shuffle (shuffle')
import qualified Total
import qualified Turn

-- | Given the hand, the permutations to consider for playing this hand
applyDifficulty :: forall a. Ord a => Difficulty -> StdGen -> [a] -> [[a]]
applyDifficulty difficulty stdgen hand =
  case (difficulty, hand) of
    (_, []) -> [] -- shuffle' doesn't support []
    (Easy, _) -> [shuffle' hand (length hand) stdgen]
    (Medium, _) -> go 4
    (Hard, _) -> go 5
  where
    go (maxFactorial :: Nat) =
      removeDups $
        ( let (start, end) = splitAt (natToInt maxFactorial) hand
           in permutations start & map (\start -> start ++ end)
        )
    -- removeDups removes duplicate hands sequences, for example [Sk, Sk] and
    -- [Sk, Sk] which can happen when the hand has the same Skeleton card twice
    removeDups :: [[a]] -> [[a]]
    removeDups decks =
      go Set.empty decks
      where
        go :: Set [(a, Nat)] -> [[a]] -> [[a]]
        go _ [] = []
        go seqs (deck : decks) | (seq deck) `Set.member` seqs = go seqs decks
        go seqs (deck : decks) = deck : go (Set.insert (seq deck) seqs) decks
        -- seq [a, b, b, a] is [(a, 1), (b, 2), (a, 1)]
        seq :: [a] -> [(a, Nat)]
        seq [] = []
        seq (x : xs) = seq_go x 1 xs
          where
            seq_go prev count [] = [(prev, count)]
            seq_go prev count (y : ys)
              | y == prev = seq_go prev (count + 1) ys
              | otherwise = (prev, count) : seq ys

-- | Smart play events
play ::
  Difficulty ->
  Shared.Model ->
  Board.T 'Core ->
  -- | The playing player
  Spots.Player ->
  -- | Events generated for player 'pSpot'
  [Game.Place]
play difficulty shared board pSpot =
  case scores of
    [] -> []
    (_, events) : _ -> events
  where
    _availMana = Board.toPart board pSpot & Board.mana
    hands :: [[Card.ID]] =
      Board.getpk @'Board.Hand pSpot board
        & map (Shared.unsafeIdentToCard shared)
        -- TODO @smelc don't do this filtering once there are cards to gain mana
        -- & filter (\card -> (Card.toCommon card & Card.mana) <= availMana)
        & map Card.unlift
        & sortOn scoreHandCard
        & map cardToIdentifier
        & applyDifficulty difficulty (Shared.getStdGen shared)
    possibles :: [[Game.Place]] =
      map
        (\hand -> playHand shared (Board.setpk @'Board.Hand pSpot hand board) pSpot)
        hands
    scores :: [(Nat, [Game.Place])] =
      possibles
        & map playAll
        & catMaybes
        & sortByFst
    playAll events =
      -- TODO @smelc write a test the 'Left' case doesn't occur often
      case Game.playAll shared $ Game.Playable board (map Game.PEvent events) undefined of
        Left msg -> traceShow ("Cannot playAll AI-generated event: " ++ Text.unpack msg) Nothing
        Right (Game.Result {board = board'}) -> Just (boardPlayerScore board' pSpot, events)

-- | The score of given player's in-place cards. Smaller is the best.
-- Both negative and positive values are returned.
boardPlayerScore :: Board.T 'Core -> Spots.Player -> Nat
boardPlayerScore board pSpot =
  sum scores
  where
    cSpotAndMayCreatures = Board.toPlayerHoleyInPlace board pSpot
    scores =
      map
        (\(cSpot, mayC) -> maybe 5 (\c -> scorePlace board c pSpot cSpot) mayC) -- Empty spot: malus of 5
        cSpotAndMayCreatures

-- | Events for playing all cards of the hand, in order. Each card
-- is played optimally.
playHand ::
  Shared.Model ->
  Board.T 'Core ->
  -- | The playing player
  Spots.Player ->
  [Game.Place]
playHand shared board pSpot =
  case aiPlayFirst shared board pSpot of
    Nothing -> []
    Just (f, s, t) ->
      let place = Game.Place' f s t
       in case Game.playAll shared $ Game.Playable board [Game.PEvent place] undefined of
            Right (Game.Result {board = b', shared = shared'}) ->
              place : playHand shared' b' pSpot
            Left msg ->
              -- This is an error, because if 'aiPlayFirst' returned 'Just card'
              -- then 'card' must be playable successfully.
              error $ "Cannot play first card of hand: " ++ Text.unpack msg ++ ". Skipping it."

-- | Take the hand's first card (if any) and return a [Place] event
-- for best placing this card.
aiPlayFirst ::
  Shared.Model ->
  Board.T 'Core ->
  -- | The playing player, i.e. the player whose hand should the
  -- the card be picked from.
  Spots.Player ->
  Maybe (Spots.Player, Game.Target, Card.ID)
aiPlayFirst shared board pSpot =
  case Board.getpk @'Board.Hand pSpot board of
    [] -> Nothing
    id : _ -> do
      let scores' = scores id & sortByFst
      -- Take all targets that have the same best score and pick one randomly
      -- Should I returned a mutated Shared.Model?
      target <- takeBestOnes scores' & Random.pick shared & fst
      return (pSpot, target, id)
  where
    handIndex = Board.HandIndex 0
    scores :: ID -> [(Nat, Game.Target)] = \id ->
      [ Game.maybePlay shared (Game.Playable board (Game.PEvent (Game.Place pSpot target handIndex)) undefined) -- Compute next board
          <&> (\(_, b, _me) -> (boardPlayerScore b pSpot, target)) -- FIXME Use '_me'
        | target <- targets board pSpot id
      ]
        & catMaybes
    takeBestOnes :: Eq a => [(a, b)] -> [b]
    takeBestOnes = \case
      [] -> [] -- No input, no output
      ((score, elem) : tail) -> elem : go score tail -- Take score from first element
      where
        -- go 0 [(0, "a"), (0, "b"), (-1, "b")] returns ["a", "b"]
        go :: Eq a => a -> [(a, b)] -> [b]
        go _ [] = []
        go searched ((score, elem) : tail) | score == searched = elem : go searched tail
        go _ _ = [] -- Because list is sorted, if first score doesn't match, then stop

targets ::
  Board.T 'Core ->
  -- | The player placing a card
  Spots.Player ->
  -- | The card being played
  Card.ID ->
  -- | All spots where the card can be put
  [Game.Target]
targets board playingPlayer id =
  case (Card.targetType id, Game.whichPlayerTarget id) of
    (CardTargetType ctk, Game.Playing) ->
      cardTargets playingPlayer ctk
    (CardTargetType ctk, Game.Opponent) ->
      cardTargets (Spots.other playingPlayer) ctk
    (PlayerTargetType, Game.Playing) ->
      [Game.PlayerTarget playingPlayer]
    (PlayerTargetType, Game.Opponent) ->
      [Game.PlayerTarget $ Spots.other playingPlayer]
  where
    cardTargets pSpot ctk =
      Board.toPlayerCardSpots board pSpot ctk & map (Game.CardTarget pSpot)

-- | The score of the card at the given position
scorePlace ::
  Board.T 'Core ->
  -- | The creature at 'pSpot' 'cSpot'
  Creature 'Core ->
  -- | Where to place the creature
  Spots.Player ->
  -- | Where to place the creature
  Spots.Card ->
  -- | The score of the card at pSpot cSpot. Smaller is the best.
  Nat
scorePlace board inPlace@Creature {attack} pSpot cSpot =
  assert (creature ~=~ inPlace) $ lineMalus `minusNatClamped` items
  where
    (~=~) Nothing _ = False
    (~=~) (Just a) b = a == b
    creature :: Maybe (Creature 'Core) = Board.toInPlaceCreature board pSpot cSpot
    cSkills = skills inPlace
    prefersBack =
      any (`elem` [Skill.Imprecise, Skill.Support, Skill.Ranged]) cSkills
        || attack == mempty
    prefersFront = any (`elem` [Skill.Charge]) cSkills
    lineMalus =
      case (inTheBack cSpot, prefersBack, prefersFront) of
        (True, True, _) -> 0
        (False, _, True) -> 0
        _ -> 2
    items = scoreCreatureItems board inPlace pSpot cSpot

-- | The score of the items of this creature (which is on the passed spot).
-- 0 is the worst. Higher values are better. The spots are where
-- the creature is.
scoreCreatureItems :: Board.T 'Core -> Creature 'Core -> Spots.Player -> Spots.Card -> Nat
scoreCreatureItems board c@Creature {attack, hp, items} pSpot cSpot =
  sum $ map scoreCreatureItem items
  where
    scoreCreatureItem :: Item -> Nat = \case
      AxeOfRage -> score attack
      BowOfStrength -> undefined
      CloakOfGaia -> undefined
      Crown ->
        levelUpBonus + positionBonus
        where
          levelUpBonus = if Total.isDisciplined c then 0 else 2
          positionBonus =
            if (Board.neighbors Board.Cardinal cSpot & length) == 3
              then 1 -- Creature is in a central spot
              else 0
      CrushingMace -> preferStrongCreature -- TODO @smelc Favor front line or not ranged at least
      FlailOfTheDamned -> preferStrongCreature
      SkBanner ->
        backBonus + skNeighborsBonus
        where
          backBonus = if Spots.inTheBack cSpot then 2 else 0 -- Prefer banner bearer to be in the back
          skNeighborsBonus =
            Board.toInPlace board pSpot
              & Map.elems
              & filter (Card.isSkeleton . Card.creatureId)
              & natLength
      SpikyMace -> preferStrongCreature -- TODO @smelc check opponent has many neighbors
      SwordOfMight -> preferStrongCreature -- TODO @smelc Favor front line or not ranged at least
    preferStrongCreature = score attack + hp

-- | The score of something. The higher the better.
-- XXX @smelc Use this class more often in this file
class Score a where
  score :: a -> Nat

instance Score Damage where
  score (Damage {base, variance}) = base + variance -- TODO @smelc, change to base + (variance `div` 2)

-- | The score of a card. Most powerful cards return a smaller value.
-- Negative values returned. FIXME @smelc Return positive values
-- Used to play most powerful cards first.
scoreHandCard :: Card 'Core -> Int
scoreHandCard = \case
  CreatureCard _ Creature {..} ->
    sum $ [-(natToInt hp), -(natToInt $ score attack)] ++ map scoreSkill skills
  NeutralCard _ NeutralObject {neutral} ->
    case neutral of
      Health -> -1
      HuntingHorn -> -1
      InfernalHaste -> -10
      Life -> -3
      Pandemonium -> -3 -- TODO @smelc pass the board and check number of in place creatures in opponent board
      Plague -> -5 -- TODO @smelc pass the board and check number of creates, for a dynamic value to be returned
      StrengthPot -> -3
  ItemCard _ ItemObject {item} ->
    case item of
      AxeOfRage -> -1
      BowOfStrength -> undefined
      CloakOfGaia -> undefined
      Crown -> -1
      CrushingMace -> -1
      FlailOfTheDamned -> -1
      SkBanner -> -1
      SpikyMace -> -1
      SwordOfMight -> -1

-- | The score of a skill, smaller values are better. Negative values returned.
scoreSkill :: Skill.State -> Int
scoreSkill s =
  case s of
    Skill.Ace -> -2
    Skill.Assassin -> -2
    Skill.Blow b -> if b then -1 else 0
    Skill.Brainless -> 2
    Skill.BreathIce -> -2
    Skill.Charge -> -1
    Skill.Discipline -> -1
    Skill.DrawCard b -> if b then -1 else 0
    Skill.Fear b -> if b then -1 else 0
    Skill.Imprecise -> 0
    Skill.Fame (n, _) -> Nat.negate n
    Skill.King -> -2
    Skill.Knight -> -1
    Skill.LongReach -> -2
    Skill.Ranged -> -1
    Skill.Regeneration n -> Nat.negate n
    Skill.Powerful -> -2
    Skill.Sadism -> -1
    Skill.Source (n, avail) -> if avail then -(natToInt n) else 0 -- TODO @smelc donc inspec avail
    Skill.Squire -> 1
    Skill.StrengthPot -> -1
    Skill.Stupid4 _ -> if Skill.isStupid s then 2 else 1
    Skill.Support -> -1
    Skill.Terror b -> if b then -2 else 0
    Skill.Unique -> 0
    Skill.Veteran -> -1
    Skill.Zealot -> -1
    _ -> error $ "unmatched: " ++ show s ++ " but this AI should be unused now"

sortByFst :: Ord a => [(a, b)] -> [(a, b)]
sortByFst l =
  sortBy sortFst l
  where
    sortFst (i, _) (j, _) = compare i j
