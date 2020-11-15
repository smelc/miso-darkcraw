{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Card where

import Control.Arrow ((&&&))
import Control.Lens
import Data.Generics.Labels ()
import Data.Kind (Constraint, Type)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

data Team = Human | Undead
  deriving (Enum, Eq, Generic, Show, Ord)

-- >>> show Undead
ppTeam :: Team -> String
ppTeam = show

allTeams :: [Team]
allTeams = [Human ..]

data Skill
  = LongReach
  | Leader
  | Ranged
  | Stubborn
  | Unique
  deriving (Eq, Generic, Ord, Show)

data SkillUI = SkillUI
  { skill :: Skill,
    skillText :: String,
    skillTitle :: String
  }
  deriving (Eq, Generic, Show)

data Phase = UI | Core

type family FilepathType (p :: Phase) where
  FilepathType UI = Filepath
  FilepathType Core = ()

type Forall (c :: Type -> Constraint) (p :: Phase) =
  ( c (FilepathType p),
    c (NeutralTeamsType p)
  )

data CreatureKind
  = Archer
  | General
  | Ghost
  | Knight
  | Mummy
  | Skeleton
  | Shade
  | Spearman
  | Swordsman
  | Vampire
  | Warrior
  deriving (Enum, Eq, Generic, Ord, Show)

data CreatureID = CreatureID {creatureKind :: CreatureKind, team :: Team}
  deriving (Eq, Generic, Ord, Show)

data Filepath = Filepath
  { root :: String,
    fpX :: Int,
    fpY :: Int
  }
  deriving (Eq, Generic, Ord, Show)

-- | The default 24x24 asset shown when an asset is not found.
-- | This makes 'creatureToFilepath' total.
default24Filepath :: Filepath
default24Filepath = Filepath {root = "24x24", fpX = 2, fpY = 3}

-- | The default 16x16 asset shown when an asset is not found.
-- | This makes 'creatureToFilepath' total.
default16Filepath :: Filepath
default16Filepath = Filepath {root = "16x16", fpX = 2, fpY = 1}

creatureToFilepath :: Maybe (Creature UI) -> Filepath
creatureToFilepath creature = maybe default24Filepath filepath creature

filepathToString :: Filepath -> String
filepathToString Filepath {..} =
  root ++ "_" ++ show fpX ++ "_" ++ show fpY ++ ".png"

data Creature (p :: Phase) = Creature
  { creatureId :: CreatureID,
    hp :: Int,
    attack :: Int,
    moral :: Maybe Int,
    victoryPoints :: Int,
    skills :: [Skill],
    filepath :: FilepathType p
  }
  deriving (Generic)

deriving instance Forall Eq p => Eq (Creature p)

deriving instance Forall Ord p => Ord (Creature p)

deriving instance Forall Show p => Show (Creature p)

data Neutral
  = Haste
  | Health
  | Life
  deriving (Eq, Generic, Ord, Show)

type family NeutralTeamsType (p :: Phase) where
  NeutralTeamsType UI = [Team]
  NeutralTeamsType Core = ()

data NeutralObject (p :: Phase) = NeutralObject
  { neutral :: Neutral,
    -- | The teams to which this neutral card applies
    neutralTeams :: NeutralTeamsType p
  }
  deriving (Generic)

deriving instance Forall Eq p => Eq (NeutralObject p)

deriving instance Forall Ord p => Ord (NeutralObject p)

deriving instance Forall Show p => Show (NeutralObject p)

data Item
  = Crown
  | FooBar
  deriving (Eq, Generic, Ord, Show)

newtype ItemObject = ItemObject
  {item :: Item}
  deriving (Generic, Show)

data Card (p :: Phase)
  = CreatureCard (Creature p)
  | NeutralCard (NeutralObject p)
  | ItemCard Item

deriving instance Forall Eq p => Eq (Card p)

deriving instance Forall Ord p => Ord (Card p)

deriving instance Forall Show p => Show (Card p)

deriving instance Generic (Card p)

unliftCreature :: Creature UI -> Creature Core
unliftCreature Creature {..} =
  Creature creatureId hp attack moral victoryPoints skills ()

unliftCard :: Card UI -> Card Core
unliftCard card =
  case card of
    CreatureCard creature -> CreatureCard $ unliftCreature creature
    NeutralCard n -> NeutralCard $ unliftNeutralObject n
    ItemCard i -> ItemCard i

unliftNeutralObject :: NeutralObject UI -> NeutralObject Core
unliftNeutralObject NeutralObject {..} =
  NeutralObject {neutral, neutralTeams = ()}

cardToCreature :: Card p -> Maybe (Creature p)
cardToCreature (CreatureCard creature) = Just creature
cardToCreature (NeutralCard _) = Nothing
cardToCreature (ItemCard _) = Nothing

-- | The minimal identifier of a card. See 'SharedModel' to obtain
-- | a full-fledged card from that.
data CardIdentifier
  = IDC CreatureID
  | IDI Item
  | IDN Neutral
  deriving (Eq, Generic, Ord, Show)

creatureToIdentifier :: Creature p -> CardIdentifier
creatureToIdentifier Creature {creatureId} = IDC creatureId

neutralToIdentifier :: NeutralObject p -> CardIdentifier
neutralToIdentifier NeutralObject {neutral} = IDN neutral

cardToIdentifier :: Card p -> CardIdentifier
cardToIdentifier card =
  case card of
    CreatureCard Creature {..} -> IDC creatureId
    ItemCard i -> IDI i
    NeutralCard NeutralObject {..} -> IDN neutral

groupCards :: [Card p] -> Map.Map CardIdentifier [Card p]
groupCards xs = Map.fromListWith (++) [(cardToIdentifier x, [x]) | x <- xs]

-- | The creature 'id' taken from 'cards'
unsafeCreatureWithID :: [Card UI] -> CreatureID -> Creature Core
unsafeCreatureWithID cards id =
  head $ filter (\c -> creatureId c == id) creatures
  where
    creatures :: [Creature Core] =
      cards ^.. folded . #_CreatureCard . to unliftCreature

unsafeCardToCreature :: Card p -> Creature p
unsafeCardToCreature (CreatureCard creature) = creature
unsafeCardToCreature (NeutralCard _) = error "neutral card not handled yet"
unsafeCardToCreature (ItemCard _) = error "item card not handled yet"

teamDeck ::
  -- | The cards as loaded from disk
  [Card UI] ->
  -- | The team for which to build the deck
  Team ->
  -- | The initial deck
  [Card Core]
teamDeck cards t =
  map CreatureCard $
    case t of
      Human -> 3 * Spearman ++ 2 * Archer ++ 1 * Knight ++ 1 * General
      Undead -> 3 * Skeleton ++ 2 * Archer ++ 1 * Mummy ++ 1 * Vampire
  where
    creatures :: Map.Map CreatureKind (Creature Core) =
      (cards ^.. folded . #_CreatureCard . to unliftCreature)
        & filter (\c -> (creatureId c & team) == t)
        & map ((creatureKind . creatureId) &&& id)
        & Map.fromList
    finder x = creatures Map.! x
    (*) i k = replicate i $ finder k
