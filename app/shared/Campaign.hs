{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- This module deals with the succession of matches
-- |
module Campaign
  ( Journey (..),
    Level (..),
    Outcome (..),
    mkJourney,
    succ,
    unsafeJourney,
  )
where

import Card (Team (..))
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map as Map
import GHC.Generics
import Theme (Theme)
import qualified Theme
import Prelude hiding (pred, succ)

-- | The levels: 'Level0' is the first level (no reward was given yet),
-- 'Level1' is the second level; etc.
data Level
  = Level0
  | Level1
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

-- | For every level, the opponent and the theme. Maps of this type
-- are complete: the domain is the entire of 'Level'
newtype Journey = Journey (Map.Map Level (Team, Theme))
  deriving (Eq, Generic, Show)

-- | @mkJourney team@ returns a journey for when the player
-- plays @team@. In the future it will likely be randomized.
mkJourney :: Team -> Journey
mkJourney team =
  Journey $
    Map.fromList $
      zip [minBound ..] $
        map (Bifunctor.second Theme.kindToTheme) $ opponents team
  where
    opponents t =
      case t of
        Evil -> [(Human, Theme.Forest), (Undead, Theme.Forest), (ZKnights, Theme.Forest)]
        Human -> [(Undead, Theme.Forest), (Evil, Theme.Forest), (ZKnights, Theme.Forest)]
        Sylvan -> [(Evil, Theme.Forest), (Undead, Theme.Forest), (ZKnights, Theme.Forest)]
        Undead -> [(Human, Theme.Forest), (Evil, Theme.Forest), (ZKnights, Theme.Forest)]
        -- Now onto teams for which we don't really care, because they are not playable
        Beastmen -> opponents Human
        ZKnights -> opponents Human

-- | An incomplete journey, but fine for playing one game at the given level,
-- against the given team.
unsafeJourney :: Campaign.Level -> Team -> Journey
unsafeJourney level opponent =
  Journey $ Map.fromList [(level, (opponent, Theme.kindToTheme Theme.Forest))]

-- | The outcome of playing a single game
data Outcome
  = -- | Player wins
    Win
  | -- | Draw
    Draw
  | -- | Player loses
    Loss
  deriving (Bounded, Enum, Generic, Show)

-- | Given a level, its successor
succ :: Level -> Maybe Level
succ Level0 = Just Level1
succ Level1 = Nothing
