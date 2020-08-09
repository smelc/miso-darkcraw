{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Card where

import Control.Arrow ((&&&))
import Control.Lens
import Data.Generics.Labels
import Data.Kind (Constraint, Type)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import GHC.Generics (Generic)

data Team = Human | Undead
  deriving (Enum, Eq, Generic, Show, Ord)

ppTeam :: Team -> String
ppTeam = show

allTeams :: [Team]
allTeams = [Human ..]

data Skill
  = HitFromBack
  | Leader
  | Ranged
  | Unique
  deriving (Eq, Generic, Ord, Show)

data Phase = UI | Core

type family CoordType (p :: Phase) where
  CoordType UI = Int
  CoordType Core = ()

type Forall (c :: Type -> Constraint) (p :: Phase) =
  ( c (CoordType p)
  )

data CreatureKind
  = Spearman
  | Swordsman
  | Archer
  | General
  | Skeleton
  | Vampire
  | Mummy
  deriving (Enum, Eq, Generic, Ord, Show)

data CreatureID = CreatureID {creatureKind :: CreatureKind, team :: Team}
  deriving (Eq, Generic, Ord, Show)

data Creature (p :: Phase) = Creature
  { creatureId :: CreatureID,
    hp :: Int,
    attack :: Int,
    moral :: Maybe Int,
    victoryPoints :: Int,
    skills :: Maybe [Skill],
    x :: CoordType p,
    y :: CoordType p,
    filename :: String
  }
  deriving (Generic)

deriving instance Forall Eq p => Eq (Creature p)

deriving instance Forall Ord p => Ord (Creature p)

deriving instance Forall Show p => Show (Creature p)

data Neutral
  = Health
  | Life
  deriving (Eq, Generic, Ord, Show)

newtype NeutralObject = NeutralObject
  {neutral :: Neutral}
  deriving (Eq, Generic, Show)

data Item
  = Crown
  | FooBar
  deriving (Eq, Generic, Ord, Show)

newtype ItemObject = ItemObject
  {item :: Item}
  deriving (Generic, Show)

data Card (p :: Phase)
  = CreatureCard (Creature p)
  | NeutralCard Neutral
  | ItemCard Item

deriving instance Forall Eq p => Eq (Card p)

deriving instance Forall Ord p => Ord (Card p)

deriving instance Forall Show p => Show (Card p)

deriving instance Generic (Card p)

creatureUI2CreatureCore :: Creature UI -> Creature Core
creatureUI2CreatureCore Creature {..} =
  Creature creatureId hp attack moral victoryPoints skills () () filename

cardToCreature :: Card p -> Maybe (Creature p)
cardToCreature (CreatureCard creature) = Just creature
cardToCreature (NeutralCard _) = Nothing
cardToCreature (ItemCard _) = Nothing

-- | The creature 'id' taken from 'cards'
unsafeCreatureWithID :: [Card UI] -> CreatureID -> Creature Core
unsafeCreatureWithID cards id =
  head $ filter (\c -> creatureId c == id) creatures
  where
    creatures :: [Creature Core] =
      cards ^.. folded . #_CreatureCard . to creatureUI2CreatureCore

unsafeCardToCreature :: Card p -> Creature p
unsafeCardToCreature (CreatureCard creature) = creature
unsafeCardToCreature (NeutralCard _) = error "neutral card not handled yet"
unsafeCardToCreature (ItemCard _) = error "item card not handled yet"

initialDeck ::
  -- | The cards as loaded from disk
  [Card UI] ->
  -- | The team for which to build the deck
  Team ->
  -- | The initial deck
  [Card Core]
initialDeck cards t =
  map CreatureCard $
    case t of
      Human -> 3 * Spearman ++ 2 * Archer ++ 1 * Spearman ++ 1 * General
      Undead -> 3 * Skeleton ++ 2 * Archer ++ 1 * Mummy ++ 1 * Vampire
  where
    creatures :: Map.Map CreatureKind (Creature Core) =
      (cards ^.. folded . #_CreatureCard . to creatureUI2CreatureCore)
        & filter (\c -> (creatureId c & team) == t)
        & map ((creatureKind . creatureId) &&& id)
        & Map.fromList
    finder x = creatures Map.!? x & fromJust
    (*) i k = replicate i $ finder k
