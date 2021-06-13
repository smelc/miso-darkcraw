-- |
-- This module deals with the succession of matches
-- |
module Campaign (Level (..), rewards, reward, decks) where

import Card (Team (..))
import qualified Card
import Prelude hiding (pred, succ)

-- | The levels: 'Level0' is the first level (no reward was given yet),
-- 'Level1' is the second level; etc.
data Level
  = Level0
  | Level1
  deriving (Show)

-- | Given a level, its predecessor
pred :: Level -> Maybe Level
pred Level0 = Nothing
pred Level1 = Just Level0

-- | Given a level, its successor
-- succ :: Level -> Maybe Level
-- succ Level0 = Just Level1
-- succ Level1 = Nothing

-- | The rewards when finishing a level
rewards :: Level -> Team -> [Card.ID]
rewards level team =
  case (level, team) of
    (Level0, Human) -> [mkCreatureID Card.Knight Human]
    (Level1, Human) -> []
    (Level0, Undead) -> []
    (Level1, Undead) -> []
  where
    mkCreatureID kind team = Card.IDC (Card.CreatureID kind team) []

-- | Given a deck, decks that can be obtained after getting
-- the reward of this level
reward :: Level -> Team -> [Card.ID] -> [[Card.ID]]
reward level team deck =
  [reward : deck | reward <- rewards level team]

-- | Given the initial deck, all decks that are possible at the given level
decks :: [Card.ID] -> Team -> Level -> [[Card.ID]]
decks deck team level =
  case pred level of
    Nothing -> [deck]
    Just predLevel ->
      concat $ [decks d team predLevel | d <- extensions]
      where
        extensions = [reward : deck | reward <- rewards level team]
