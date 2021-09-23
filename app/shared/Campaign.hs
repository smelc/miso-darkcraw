{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- This module deals with the succession of matches
-- |
module Campaign (Level (..), Outcome (..), augment, loot, nbRewards, succ) where

import Card (Team (..))
import qualified Card
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import GHC.Generics
import Nat
import Prelude hiding (pred, succ)

-- | The levels: 'Level0' is the first level (no reward was given yet),
-- 'Level1' is the second level; etc.
data Level
  = Level0
  | Level1
  deriving (Eq, Generic, Show)

-- | The outcome of playing a single game
data Outcome
  = -- | Player wins
    Win
  | -- | Draw
    Draw
  | -- | Player loses
    Loss
  deriving (Bounded, Enum, Generic, Show)

-- | Given a level, its predecessor
pred :: Level -> Maybe Level
pred Level0 = Nothing
pred Level1 = Just Level0

-- | Given a level, all levels before it
preds :: Level -> [Level]
preds level =
  reverse $ go level
  where
    go level =
      case pred level of
        Nothing -> []
        Just predLevel -> predLevel : go predLevel

-- | Given a level, its successor
succ :: Level -> Maybe Level
succ Level0 = Just Level1
succ Level1 = Nothing

-- | The number of rewards that have been obtained when playing the given level
nbRewards :: Level -> Nat
nbRewards level =
  case pred level of
    Nothing -> 0
    Just predLevel -> 1 + nbRewards predLevel

-- | The possible rewards when finishing a level
rewards :: Level -> Team -> [Card.ID]
rewards level team =
  case (level, team) of
    (_, Evil) -> []
    (Level0, Human) -> map (mkIDC team) [Card.Knight] ++ map Card.IDI [Card.Crown]
    (Level1, Human) -> map (mkIDC team) [Card.Ogre]
    (Level0, Undead) -> map (mkIDC team) [Card.Necromancer, Card.Specter]
    (Level1, Undead) -> [Card.IDI Card.SkBanner]
  where
    mkIDC team kind = Card.IDC (Card.CreatureID kind team) []

-- | The possible rewards when finishing the given 'Level' with the given 'Outcome'
loot :: Outcome -> Level -> Team -> [Card.ID]
loot Win level team = rewards level team
loot Draw level team =
  altTake True (win, loss) & take (length win)
  where
    win = loot Win level team
    loss = loot Loss level team
    altTake :: Bool -> ([a], [a]) -> [a]
    altTake _ ([], []) = []
    altTake True (firstWin : restWin, loss) = firstWin : altTake False (restWin, loss)
    altTake False (win, firstLoss : restLoss) = firstLoss : altTake True (win, restLoss)
    altTake _ ([], loss) = loss
    altTake _ (win, []) = win
loot Loss level team =
  case pred level of
    Nothing -> rewards level team & (\l -> if length l > 1 then drop 1 l else l)
    Just levelb -> rewards levelb team

-- | All possible rewards that can have been obtained from the start,
-- when playing the given level
rewardsUpTo :: Level -> Team -> [[Card.ID]]
rewardsUpTo level team =
  fromMaybe [] $ go rewardsSeq
  where
    rewardsSeq = preds level & map (`rewards` team)
    go :: [[Card.ID]] -> Maybe [[Card.ID]]
    go [] = Nothing
    go (rewardsAtLevel : nextRewards) =
      case go nextRewards of
        Nothing ->
          Just [[rewardAtLevel] | rewardAtLevel <- rewardsAtLevel]
        Just nextRewards ->
          Just
            [ rewardAtLevel : next
              | rewardAtLevel <- rewardsAtLevel,
                next <- nextRewards
            ]

-- | Given the current deck, all decks that are possible at the given level
augment :: [Card.ID] -> Level -> Team -> [[Card.ID]]
augment deck team level =
  case rewardsUpTo team level of
    [] -> [deck]
    _ -> [rewards ++ deck | rewards <- rewardsUpTo team level]
