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
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Kind (Constraint, Type)
import qualified Data.Map.Strict as Map
import Data.Maybe
import GHC.Generics (Generic)
import Tile

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

data Phase
  = -- | Phase for data in core algorithms ('AI', 'Game'); not for rendering.
    -- Creatures in this phase have actual hitpoints, actual attack, etc.
    Core
  | -- | Phase for data in UI algorithms: contains more data related
    -- to drawing cards. Data in this phase is _formal_, i.e. hitpoints
    -- are the maximum (pristine) hitpoints, attack is formal attack (before
    -- maluses or bonuses), etc.
    UI

type family TextType (p :: Phase) where
  TextType UI = String
  TextType Core = ()

type family TileType (p :: Phase) where
  TileType UI = Tile
  TileType Core = ()

type Forall (c :: Type -> Constraint) (p :: Phase) =
  ( c (TextType p),
    c (TileType p),
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

-- creatureToFilepath :: Maybe (Creature UI) -> Filepath
-- creatureToFilepath creature = maybe default24Filepath filepath creature

data Creature (p :: Phase) = Creature
  { creatureId :: CreatureID,
    hp :: Int,
    attack :: Int,
    moral :: Maybe Int,
    victoryPoints :: Int,
    skills :: [Skill],
    tile :: TileType p
  }
  deriving (Generic)

deriving instance Forall Eq p => Eq (Creature p)

deriving instance Forall Ord p => Ord (Creature p)

deriving instance Forall Show p => Show (Creature p)

data Neutral
  = Health
  | InfernalHaste
  | Life
  deriving (Eq, Generic, Ord, Show)

type family NeutralTeamsType (p :: Phase) where
  NeutralTeamsType UI = [Team]
  NeutralTeamsType Core = ()

-- If Creature and NeutralObject start having more in common than solely
-- tile/ntile, a new record can be introduced; to share code.

data NeutralObject (p :: Phase) = NeutralObject
  { neutral :: Neutral,
    -- | The teams to which this neutral card applies
    neutralTeams :: NeutralTeamsType p,
    ntext :: TextType p,
    ntile :: TileType p,
    ntitle :: TextType p
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
  NeutralObject {neutral, neutralTeams = (), ntext = (), ntile = (), ntitle = ()}

cardToCreature :: Card p -> Maybe (Creature p)
cardToCreature (CreatureCard creature) = Just creature
cardToCreature (NeutralCard _) = Nothing
cardToCreature (ItemCard _) = Nothing

cardToNeutralObject :: Card p -> Maybe (NeutralObject p)
cardToNeutralObject (NeutralCard n) = Just n
cardToNeutralObject (CreatureCard _) = Nothing
cardToNeutralObject (ItemCard _) = Nothing

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

identToId :: CardIdentifier -> Maybe CreatureID
identToId (IDC cid) = Just cid
identToId _ = Nothing

groupCards :: [Card p] -> Map.Map CardIdentifier [Card p]
groupCards xs = Map.fromListWith (++) [(cardToIdentifier x, [x]) | x <- xs]

teamDeck ::
  -- | The cards as loaded from disk
  [Card UI] ->
  -- | The team for which to build the deck
  Team ->
  -- | The initial deck
  [Card Core]
teamDeck cards t =
  map CreatureCard creatures ++ map NeutralCard neutrals
  where
    kindToCreature :: Map.Map CreatureKind (Creature Core) =
      map cardToCreature cards
        & catMaybes
        & filter (\c -> (creatureId c & team) == t)
        & map unliftCreature
        & map ((creatureKind . creatureId) &&& id)
        & Map.fromList
    (*) i k = replicate i $ kindToCreature Map.! k
    -- Initial creatures:
    creatures =
      case t of
        Human -> 3 * Spearman ++ 2 * Archer ++ 1 * Knight ++ 1 * General
        Undead -> 3 * Skeleton ++ 2 * Archer ++ 1 * Mummy ++ 1 * Vampire
    kindToNeutral :: Map.Map Neutral (NeutralObject Core) =
      map cardToNeutralObject cards
        & catMaybes
        & filter (\nobj -> t `Prelude.elem` neutralTeams nobj)
        & map unliftNeutralObject
        & map (\nobj -> (neutral nobj, nobj))
        & Map.fromList
    (**) i k = replicate i $ kindToNeutral Map.! k
    neutrals =
      case t of
        Human -> 1 ** Health ++ 1 ** Life
        Undead -> 2 ** InfernalHaste

-- | Whether a neutral card can be applied to the given creature
appliesTo :: Neutral -> Creature Core -> Bool
appliesTo n _ =
  case n of
    Health -> True
    Life -> True
    InfernalHaste -> False
