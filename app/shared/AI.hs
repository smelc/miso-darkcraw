{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This module defines how the AI plays.
-- 'applyDifficulty' and 'boardScore' are exported for tests
module AI (applyDifficulty, AI.play, boardPlayerScore, Difficulty (..), placeCards) where

import Board
  ( Board,
    HandIndex (HandIndex),
    PlayerSpot,
  )
import qualified Board
import BoardInstances ()
import Card
import Control.Exception
import Control.Lens hiding (snoc)
import Damage (Damage (..))
import Data.Either.Extra
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Debug.Trace (trace, traceShow)
import GHC.Generics (Generic)
import Game hiding (Event, Result)
import qualified Game
import Nat
import SharedModel (SharedModel)
import qualified SharedModel
import qualified Skill
import Spots
import System.Random (StdGen)
import System.Random.Shuffle (shuffle')
import qualified Total

-- | The AI's level
data Difficulty
  = Easy
  | Medium
  | Hard
  deriving (Eq, Generic, Show)

-- | Events that place creatures on the board. This function guarantees
-- that the returned events are solely placements (no neutral cards), so
-- that playing them with 'Game.playAll' returns an empty list of 'Game.Event'
placeCards ::
  Difficulty ->
  SharedModel ->
  Board 'Core ->
  -- | The player whose cards must be played
  PlayerSpot ->
  [Game.Event]
placeCards difficulty shared board turn =
  -- Will fail once when we do more stuff in aiPlay. It's OK, I'll
  -- adapt when this happens.
  assert (all isPlaceEvent events) events
  where
    events = AI.play difficulty shared board turn
    isPlaceEvent = \case
      ApplyChurch {} -> False
      ApplyFearNTerror {} -> False
      ApplyKing {} -> False
      Attack {} -> False
      FillTheFrontline {} -> False
      NoPlayEvent -> False
      Place {} -> True
      Place' {} -> True

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
  SharedModel ->
  Board 'Core ->
  -- | The playing player
  PlayerSpot ->
  -- | Events generated for player 'pSpot'
  [Game.Event]
play difficulty shared board pSpot =
  case scores of
    [] -> []
    (_, events) : _ -> events
  where
    availMana = Board.toPart board pSpot & Board.mana
    hands :: [[Card.ID]] =
      Board.toHand board pSpot
        & map (SharedModel.unsafeIdentToCard shared)
        & filter (\card -> (Card.toCommon card & Card.mana) <= availMana)
        & map Card.unlift
        & sortOn scoreHandCard
        & map cardToIdentifier
        & applyDifficulty difficulty (SharedModel.getStdGen shared)
    possibles =
      map
        (\hand -> playHand shared (Board.setHand board pSpot hand) pSpot)
        hands
    scores :: [(Int, [Game.Event])] =
      map
        ( \events ->
            case Game.playAll shared board events of
              Left msg -> trace ("Maybe unexpected? " ++ Text.unpack msg) Nothing
              Right (Game.PolyResult _ board' () _) -> Just (boardPlayerScore board' pSpot, events)
        )
        possibles
        & catMaybes
        & sortByFst

-- | The score of given player's in-place cards. Smaller is the best.
-- Both negative and positive values are returned.
boardPlayerScore :: Board 'Core -> PlayerSpot -> Int
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
  SharedModel ->
  Board 'Core ->
  -- | The playing player
  PlayerSpot ->
  [Game.Event]
playHand shared board pSpot =
  case aiPlayFirst shared board pSpot of
    Nothing -> []
    Just (f, s, t) ->
      case Game.playAll shared board [event] of
        Right (Game.PolyResult shared' b' () _) -> event : playHand shared' b' pSpot
        Left msg ->
          traceShow ("Cannot play first card of hand: " ++ Text.unpack msg ++ ". Skipping it.") $
            playHand shared board' pSpot
          where
            -- The call to tail is safe because the hand must be non-empty,
            -- by aiPlayFirst returning Just _
            hand' = Board.toHand board pSpot & tail
            board' = Board.setHand board pSpot hand' -- Skip first card
      where
        event = Place' f s t

-- | Take the hand's first card (if any) and return a [Place] event
-- for best placing this card.
aiPlayFirst ::
  SharedModel ->
  Board 'Core ->
  -- | The playing player, i.e. the player whose hand should the
  -- the card be picked from.
  PlayerSpot ->
  Maybe (PlayerSpot, Target, Card.ID)
aiPlayFirst shared board pSpot =
  case Board.toHand board pSpot of
    [] -> Nothing
    id : _ -> do
      let scores' = scores id & sortByFst
      -- Take all targets that have the same best score and pick one randomly
      -- Should I returned a mutated SharedModel?
      target <- takeBestOnes scores' & SharedModel.shuffle shared & snd & listToMaybe
      return (pSpot, target, id)
  where
    handIndex = HandIndex 0
    scores :: ID -> [(Int, Target)] = \id ->
      [ (board' & eitherToMaybe <&> (\(Game.PolyResult _ b _ _) -> boardPlayerScore b pSpot), target)
        | target <- targets board pSpot id,
          let board' = Game.play shared board $ Place pSpot target handIndex
      ]
        & map liftMaybe
        & catMaybes
    liftMaybe (Nothing, _) = Nothing
    liftMaybe (Just x, y) = Just (x, y)
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
  Board 'Core ->
  -- | The player placing a card
  PlayerSpot ->
  -- | The card being played
  Card.ID ->
  -- | All spots where the card can be put
  [Target]
targets board playingPlayer id =
  case (Card.targetType id, whichPlayerTarget id) of
    (CardTargetType ctk, Game.Playing) ->
      cardTargets playingPlayer ctk
    (CardTargetType ctk, Opponent) ->
      cardTargets (otherPlayerSpot playingPlayer) ctk
    (PlayerTargetType, Playing) ->
      [PlayerTarget playingPlayer]
    (PlayerTargetType, Opponent) ->
      [PlayerTarget $ otherPlayerSpot playingPlayer]
  where
    cardTargets pSpot ctk =
      Board.toPlayerCardSpots board pSpot ctk & map (CardTarget pSpot)

-- | The score of the card at the given position
scorePlace ::
  Board 'Core ->
  -- | The creature at 'pSpot' 'cSpot'
  Creature 'Core ->
  -- | Where to place the creature
  PlayerSpot ->
  -- | Where to place the creature
  CardSpot ->
  -- | The score of the card at pSpot cSpot. Smaller is the best.
  -- Both negative and positive values are returned.
  Int
scorePlace board inPlace pSpot cSpot =
  assert (creature ~=~ inPlace) result
  where
    (~=~) Nothing _ = False
    (~=~) (Just a) b = a == b
    creature :: Maybe (Creature 'Core) = Board.toInPlaceCreature board pSpot cSpot
    enemiesInPlace :: Map.Map CardSpot (Creature 'Core) =
      Board.toInPlace board (otherPlayerSpot pSpot)
    cSkills = skills inPlace
    prefersBack = Skill.Ranged `elem` cSkills || Skill.LongReach `elem` cSkills
    lineMalus = if inTheBack cSpot == prefersBack then 0 else 1
    enemySpots' :: [CardSpot] = allEnemySpots cSpot
    enemiesInColumn = map (enemiesInPlace Map.!?) enemySpots'
    -- TODO @smelc instead, play EndTurn pSpot cSpot and look at the score
    -- increase. This will avoid doing an incorrect simulation.
    yieldsVictoryPoints = all isNothing enemiesInColumn
    victoryPointsMalus = if yieldsVictoryPoints then 0 else 1
    maluses :: Nat = sum [lineMalus, victoryPointsMalus]
    result = (natToInt maluses) - (natToInt $ scoreCreatureItems board inPlace pSpot cSpot)

-- | The score of the items of this creature (which is on the passed spot).
-- 0 is the worst. Higher values are better. The spots are where
-- the creature is.
scoreCreatureItems :: Board 'Core -> Creature 'Core -> PlayerSpot -> CardSpot -> Nat
scoreCreatureItems board c@Creature {attack, hp, items} pSpot cSpot =
  sum $ map scoreCreatureItem items
  where
    scoreCreatureItem :: Item -> Nat = \case
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
      SwordOfMight -> preferStrongCreature -- TODO @smelc Favor front line or not ranged at least
    preferStrongCreature = score attack + hp

-- | The score of something. The higher the better.
-- XXX @smelc Use this class more often in this file
class Score a where
  score :: a -> Nat

instance Score Damage where
  score (Damage {base, variance}) = base + variance

-- | The score of a card. Most powerful cards return a smaller value.
-- Negative values returned. FIXME @smelc Return positive values
-- Used to play most powerful cards first.
scoreHandCard :: Card 'Core -> Int
scoreHandCard = \case
  CreatureCard _ Creature {..} ->
    sum $ [- (natToInt hp), - (natToInt $ score attack)] ++ map scoreSkill skills
  NeutralCard _ NeutralObject {neutral} ->
    case neutral of
      Health -> -1
      InfernalHaste -> -10
      Life -> -3
      Plague -> -5 -- TODO @smelc pass the board and check number of creates, for a dynamic value to be returned
  ItemCard _ ItemObject {item} ->
    case item of
      Crown -> -1
      CrushingMace -> -1
      FlailOfTheDamned -> -1
      SkBanner -> -1
      SwordOfMight -> -1

-- | The score of a skill, smaller values are better. Negative values returned.
scoreSkill :: Skill.State -> Int
scoreSkill s =
  case s of
    Skill.Blow b -> if b then -1 else 0
    Skill.BreathIce -> -2
    Skill.Charge -> -1
    Skill.Discipline -> -1
    Skill.DrawCard b -> if b then -1 else 0
    Skill.Fear b -> if b then -1 else 0
    Skill.King -> -2
    Skill.Knight -> -1
    Skill.LongReach -> -1
    Skill.Ranged -> -1
    Skill.Source (n, avail) -> if avail then - (natToInt n) else 0
    Skill.Squire -> 1
    Skill.Stupid4 _ -> if Skill.isStupid s then 2 else 1
    Skill.Terror b -> if b then -2 else 0
    Skill.Unique -> 0
    Skill.Veteran -> -1
    Skill.Zealot -> -1

sortByFst :: [(Int, b)] -> [(Int, b)]
sortByFst l =
  sortBy sortFst l
  where
    sortFst (i, _) (j, _) = compare i j
