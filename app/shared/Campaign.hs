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
    anywhere,
    augment,
    fixed,
    mkJourney,
    loot,
    nbRewards,
    succ,
    unsafeJourney,
  )
where

import Card (Team (..))
import qualified Card
import qualified Data.Bifunctor as Bifunctor
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import GHC.Generics
import Nat
import System.Random (Random (..), StdGen, random)
import System.Random.Shuffle (shuffle')
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

-- | The possible rewards when finishing a level.
-- FIXME @smelc delete me, superseded by Network.rewards
rewards :: Level -> Team -> [Card.ID]
rewards level team =
  case (level, team) of
    -- Evil
    (Level0, Evil) -> [Card.IDI Card.AxeOfRage]
    (Level1, Evil) -> []
    -- Human
    (Level0, Human) -> map (mkIDC team) [Card.Knight] ++ [Card.IDI Card.Crown] ++ map Card.IDN [Card.Life]
    (Level1, Human) -> map (mkIDC team) [Card.Ogre]
    -- Sylvan
    (Level0, Sylvan) ->
      map (mkIDC Sylvan) [Card.Worm] ++ [Card.IDI Card.BowOfGaia] ++ [Card.IDN Card.HuntingHorn]
    (Level1, Sylvan) -> map Card.IDI [Card.BowOfGaia, Card.BowOfStrength]
    -- Undead
    (Level0, Undead) -> map (mkIDC team) [Card.Necromancer, Card.Specter]
    (Level1, Undead) -> [Card.IDI Card.SkBanner]
    -- Unplayable teams
    (_, Beastmen) -> [] -- Not a playable team, it's fine
    (_, ZKnights) -> [] -- Not a playable team, it's fine
  where
    mkIDC team kind = Card.IDC (Card.CreatureID kind team) []

-- | The possible rewards when finishing the given 'Level' with the given 'Outcome'
loot :: Maybe StdGen -> Outcome -> Level -> Team -> [Card.ID]
loot stdgen outcome level team =
  shuffle unshuffled
  where
    shuffle l =
      case (stdgen, l) of
        (Nothing, _) -> l
        (_, []) -> [] -- shuffle' doesn't support []
        (Just stdgen, _) -> shuffle' l (length l) stdgen
    win = rewards level team
    loss =
      case pred level of
        Nothing -> rewards level team & shuffle & (\l -> if length l > 1 then drop 1 l else l)
        Just levelb -> rewards levelb team
    unshuffled =
      case outcome of
        Win -> win
        Draw ->
          case stdgen of
            Nothing -> win
            Just stdgen -> if random stdgen & fst then win else loss
        Loss -> loss

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

-- | Teams that can be fought against at any 'Level'. See 'fixed' for
-- other possible fights.
anywhere :: [Team]
anywhere = [Human, Undead]

-- | Fights that can be done only at specific levels
fixed :: [(Team, Level)]
fixed = [(ZKnights, Level1)]
