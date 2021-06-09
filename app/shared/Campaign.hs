{-# LANGUAGE DataKinds #-}

-- |
-- This module deals with the succession of matches
-- |
module Campaign (Level (..), rewards, reward) where

import Card (Team (..))
import qualified Card

-- | The levels: 'Level0' is the first level (no reward was given yet),
-- 'Level1' is the second level; etc.
data Level
  = Level0
  | Level1

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
