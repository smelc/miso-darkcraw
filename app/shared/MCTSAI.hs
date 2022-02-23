{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCTSAI (newPlaySim) where

import Board (Board)
import qualified Board
import BoardInstances ()
import Card
import Constants
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
import Data.Ord
import Data.Tuple.Extra
import qualified Game
import qualified HeuristicAI
import qualified Move
import Nat
import SharedModel (SharedModel)
import qualified SharedModel
import qualified Skill
import Spots hiding (Card)
import qualified Turn

-- | Sorts a pair of lists, using the first member as the ordering. Usual
-- ascending sort: @[1, 3, 5, etc.].
_sortBySmallerFst :: Ord a => [(a, b)] -> [(a, b)]
_sortBySmallerFst l = sortBy (\(i, _) (j, _) -> compare i j) l

-- | Sorts a pair of lists, using the first member as the ordering.
-- Descending sort: @[10, 8, 5, 4, etc.]
sortByLargerFst :: Ord a => [(a, b)] -> [(a, b)]
sortByLargerFst l = sortBy (\(i, _) (j, _) -> compare (Down i) (Down j)) l

class NewScore a where
  -- The score of a structure. Bigger is the best
  nscore :: a -> Int

instance NewScore (Board 'Core, Spots.Player) where
  nscore (board, pSpot) =
    nscore (Board.toPart board pSpot) - nscore (Board.toPart board (Spots.other pSpot))

instance NewScore (Board.PlayerPart 'Core) where
  nscore Board.PlayerPart {inPlace, score} =
    sum (Map.elems inPlace & map nscore)
      + (Nat.natToInt score)
      + sum (map position (Map.toList inPlace))
    where
      position (cspot, Creature {items, skills})
        | Spots.inFront cspot = fronts - backs
        | otherwise = backs - fronts
        where
          (fronts, backs) =
            map preference items <> map preference skills
              & filter ((/=) NoPref)
              & partition ((==) Front)
              & both length

instance NewScore (Creature 'Core) where
  nscore Creature {attack, hp, skills} =
    Nat.natToInt hp
      + nscore attack
      + sum (map nscore skills)

instance NewScore Damage where
  nscore Damage {base, variance} = Nat.natToInt (base + (variance `div` 2))

instance NewScore Skill.State where
  nscore =
    \case
      Skill.Ace -> 2
      Skill.Assassin -> 2
      Skill.Blow _ -> 1
      Skill.Brainless -> -1
      Skill.BreathIce -> 1
      Skill.Charge -> 1
      Skill.Discipline -> 1
      Skill.DrawCard _ -> 1
      Skill.Fame _ -> 1
      Skill.Fear _ -> 1
      Skill.Imprecise -> 0
      Skill.King -> 1
      Skill.Knight -> 1
      Skill.LongReach -> 1
      Skill.Powerful -> 2
      Skill.Ranged -> 1
      Skill.Regeneration _ -> 1
      Skill.Sadism -> 1
      Skill.StrengthPot -> 2
      Skill.Source _ -> 2
      Skill.Squire -> 1
      Skill.Stupid4 _ -> -1
      Skill.Support -> 1
      Skill.Terror _ -> 2
      Skill.Unique -> 0
      Skill.Veteran -> 1
      Skill.Zealot -> 1

instance NewScore Item where
  nscore =
    \case
      AxeOfRage -> 1
      Crown -> 1
      CrushingMace -> 1
      FlailOfTheDamned -> 1
      SkBanner -> 1
      SpikyMace -> 1
      SwordOfMight -> 1

-- | The preferred position of something, line-wise
data PrefPosition
  = Back
  | Front
  | NoPref
  deriving (Eq)

class Preference a where
  preference :: a -> PrefPosition

instance Preference Skill.State where
  -- This implementation is slightly biased towards the existing cards.
  -- It's not theoretical: it's written knowing which creature has what.
  preference =
    \case
      Skill.Ace -> Back
      Skill.Assassin -> NoPref
      Skill.Brainless -> NoPref
      Skill.BreathIce -> NoPref
      Skill.Blow _ -> Front
      Skill.Charge -> Front
      Skill.Discipline -> Front
      Skill.DrawCard _ -> Back
      Skill.Fame _ -> Back
      Skill.Fear _ -> Front
      Skill.Imprecise -> Back
      Skill.King -> Back
      Skill.Knight -> Front
      Skill.LongReach -> NoPref
      Skill.Powerful -> Front
      Skill.Ranged -> Back
      Skill.Regeneration _ -> Front
      Skill.Sadism -> NoPref
      Skill.StrengthPot -> Front
      Skill.Source _ -> Back
      Skill.Squire -> Back
      Skill.Stupid4 _ -> NoPref
      Skill.Support -> Back
      Skill.Terror _ -> Front
      Skill.Unique -> NoPref
      Skill.Veteran -> Front
      Skill.Zealot -> Front

instance Preference Item where
  preference =
    \case
      AxeOfRage -> Front
      Crown -> NoPref
      CrushingMace -> Front
      FlailOfTheDamned -> NoPref
      SkBanner -> Back
      SpikyMace -> Front
      SwordOfMight -> NoPref

-- | Generated events, after having chosen from the ones given by 'newPlay',
-- by running the simulation.
newPlaySim ::
  Difficulty ->
  SharedModel ->
  -- | The spot to play
  Spots.Player ->
  -- | The board to consider
  Board 'Core ->
  [Game.Place]
newPlaySim difficulty shared pSpot board =
  case simulated & sortByLargerFst & listToMaybe of
    Nothing -> []
    Just (_score, (events, _board')) -> NE.toList events
  where
    nextSched = Move.nextify $ Just Move.EndTurnPressed
    mkKernel' = mkKernel difficulty shared pSpot
    simulated :: [(Int, (NE.NonEmpty Game.Place, Board 'Core))] =
      map (\(events, board) -> (events, Move.simRunAllMaybe nextSched (mkKernel' board) & runExcept)) events
        & mapMaybe lift
        & map (Bifunctor.second Move.board)
        & map (\pair@(_, board) -> (nscore (board, pSpot), pair))
    events = newPlay difficulty shared pSpot board
    lift (_, Left _) = Nothing
    lift (x, Right y) = Just (x, y)

-- | Make a 'Kernel' value for simulating the given board
mkKernel :: Difficulty -> SharedModel -> Player -> Board 'Core -> Move.Kernel ()
mkKernel difficulty shared pSpot =
  Move.mkSimKernel difficulty shared turn
  where
    turn = Turn.initial & (\t -> if Turn.toPlayerSpot t == pSpot then t else Turn.next t)

-- | Many possible events, with the end board after applying those events.
-- Doesn't run the simulation (no call to 'Move').
newPlay ::
  Difficulty ->
  SharedModel ->
  -- | The spot to play
  Spots.Player ->
  -- | The board to consider
  Board 'Core ->
  -- | Generated events, and the resulting board after having played those
  -- events.
  [(NE.NonEmpty Game.Place, Board 'Core)]
newPlay difficulty shared pSpot board =
  case newPlayFirst difficulty shared pSpot board of
    EmptyHand -> []
    NoMana -> []
    One (_id, pairs :: [(Game.Place, Board 'Core)]) ->
      pairs
        & map (\(place, board') -> (place, board', newPlay difficulty shared pSpot board'))
        & map
          ( \(place, board', next) ->
              if null next
                then [(place NE.:| [], board')]
                else -- If 'next' is [], the comprehension that follows is []. We don't want that, hence the 'if'
                  [(place NE.:| NE.toList rest, board'') | (rest, board'') <- next]
          )
        & concat

-- | Type returned by 'newPlayFirst', that plays a single card; taking
-- the hand in order (but possibly skipping unplayable cards)
data First
  = -- | AI is done, because there are no cards left to play
    EmptyHand
  | -- | AI is done, because playing player has no mana
    NoMana
  | -- | Played one card at the given spot, yielding the given board
    -- FIXME return the updated SharedModel
    One (Card.ID, [(Game.Place, Board 'Core)])
  deriving (Show)

-- | Plays one event and plays following events if any. The point of
-- this function is to get the next state after playing one card. If the
-- card cannot be played, 'Nothing' is returned. We cannot use 'Game.maybePlay'
-- because it doesn't enqueue the created events. We cannot use 'Game.playAll'
-- because it doesn't distinguish if the first event succeeded. If it didn't,
-- we MUST know it, so that the AI skips this event.
place :: Difficulty -> SharedModel -> Game.Place -> Board 'Core -> Maybe (Board 'Core)
place difficulty shared place board =
  case Game.maybePlay shared board (Game.PEvent place) of
    Nothing -> Nothing
    Just (_, board', Nothing) -> Just board'
    Just (shared', board', Just nextEvent) ->
      Move.simRunAllMaybe (nextEvent & Move.Play & Just & Move.nextify) kernel
        & runExcept
        & eitherToMaybe
        <&> Move.board
      where
        kernel = mkKernel difficulty shared' (Game.toSpot place) board'

newPlayFirst ::
  Difficulty ->
  SharedModel ->
  Spots.Player ->
  Board 'Core ->
  First
newPlayFirst difficulty shared pSpot board =
  case (hand, availMana) of
    (_, 0) -> NoMana
    ([], _) -> EmptyHand
    (card : rest, _) ->
      case (mana card, availTargets card) of
        (Nothing, _) ->
          error ("Mana of card not found: " ++ show card)
        (Just manaNeeded, targets)
          | availMana < manaNeeded || null targets ->
              -- Not enough mana, or no target for this card. Skip it by deleting
              -- it from the hand in a recursive call:
              newPlayFirst difficulty shared pSpot $ withCards rest
        (Just _, targets) ->
          One (card, pairs)
          where
            pairs :: [(Game.Place, Board 'Core)] =
              targets
                & map
                  ( \target ->
                      let place = Game.Place' pSpot target card
                       in (place, MCTSAI.place difficulty shared place board)
                  )
                & mapMaybe lift
            lift (x, Just y) = Just (x, y)
            lift (_x, Nothing) = Nothing
  where
    hand = Board.toHand board pSpot
    mana id = SharedModel.toCardCommon shared id <&> Card.mana
    availMana = Board.toPart board pSpot & Board.mana
    availTargets :: Card.ID -> [Game.Target] = HeuristicAI.targets board pSpot
    withCards cards = Board.setHand board pSpot cards
