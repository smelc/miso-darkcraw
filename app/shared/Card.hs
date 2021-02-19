{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Card where

import qualified Constants
import Control.Arrow ((&&&))
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Kind (Constraint, Type)
import qualified Data.Map.Strict as Map
import Data.Maybe
import GHC.Generics (Generic)
import Tile

-- If you change the first member, change 'allTeams' too
data Team = Human | Undead
  deriving (Enum, Eq, Generic, Show, Ord)

-- >>> show Undead
ppTeam :: Team -> String
ppTeam = show

allTeams :: [Team]
allTeams = [Human ..]

data Skill
  = Blow
  | Discipline
  | DrawCard
  | LongReach
  | Ranged
  | Stupid4
  | Unique
  deriving (Eq, Generic, Ord, Show)

data SkillCore
  = -- | Whether the skill is available (True) or used already (False)
    Blow' Bool
  | Discipline'
  | -- | Whether the skill is available (True) or used already (False)
    DrawCard' Bool
  | LongReach'
  | Ranged'
  | -- | The turn, at 0, 1, or 2 not stupide; at 3 stupid; then back to 0
    Stupid4' Int
  | Unique'
  deriving (Eq, Generic, Ord, Show)

isStupid :: SkillCore -> Bool
isStupid = \case Stupid4' 3 -> True; _ -> False

data SkillPack = SkillPack
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

type family ItemType (p :: Phase) where
  ItemType UI = ItemObject UI
  ItemType Core = Item

type family OffsetType (p :: Phase) where
  OffsetType UI = Int
  OffsetType Core = ()

type family SkillType (p :: Phase) where
  SkillType UI = Skill
  SkillType Core = SkillCore

type family TextType (p :: Phase) where
  TextType UI = String
  TextType Core = ()

type family TileType (p :: Phase) where
  TileType UI = Tile
  TileType Core = ()

type Forall (c :: Type -> Constraint) (p :: Phase) =
  ( c (ItemType p),
    c (OffsetType p),
    c (SkillType p),
    c (TextType p),
    c (TileType p),
    c (NeutralTeamsType p)
  )

-- If you change the first member, change 'allCreatureKinds' too
data CreatureKind
  = Archer
  | General
  | Ghost
  | Knight
  | Necromancer
  | Mummy
  | Skeleton
  | Shade
  | Spearman
  | Swordsman
  | Ogre
  | Vampire
  | Warrior
  deriving (Enum, Eq, Generic, Ord, Show)

allCreatureKinds = [Archer ..]

data CreatureID = CreatureID {creatureKind :: CreatureKind, team :: Team}
  deriving (Eq, Generic, Ord, Show)

data Creature (p :: Phase) = Creature
  { creatureId :: CreatureID,
    hp :: Int,
    -- | Beware when using this accessor, you may want 'totalAttack' instead
    attack :: Int,
    items :: [ItemType p],
    moral :: Maybe Int,
    victoryPoints :: Int,
    skills :: [SkillType p],
    tile :: TileType p
  }
  deriving (Generic)

deriving instance Forall Eq p => Eq (Creature p)

deriving instance Forall Ord p => Ord (Creature p)

deriving instance Forall Show p => Show (Creature p)

-- If you change the first member, change 'allNeutrals' too
data Neutral
  = Health
  | InfernalHaste
  | Life
  deriving (Enum, Eq, Generic, Ord, Show)

allNeutrals = [Health ..]

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

-- If you change the first member, update 'allItems'
data Item
  = Crown
  | FlailOfTheDamned
  | SwordOfMight
  deriving (Enum, Eq, Generic, Ord, Show)

allItems = [Card.Crown ..]

data ItemObject (p :: Phase) = ItemObject
  { item :: Item,
    itext :: TextType p,
    itextSzOffset :: OffsetType p,
    itile :: TileType p,
    ititle :: TextType p,
    ititleSzOffset :: OffsetType p
  }
  deriving (Generic)

deriving instance Forall Eq p => Eq (ItemObject p)

deriving instance Forall Ord p => Ord (ItemObject p)

deriving instance Forall Show p => Show (ItemObject p)

data Card (p :: Phase)
  = CreatureCard (Creature p)
  | NeutralCard (NeutralObject p)
  | ItemCard (ItemObject p)

deriving instance Forall Eq p => Eq (Card p)

deriving instance Forall Ord p => Ord (Card p)

deriving instance Forall Show p => Show (Card p)

deriving instance Generic (Card p)

liftSkill :: SkillCore -> Skill
liftSkill skill =
  case skill of
    Blow' _ -> Blow
    Discipline' -> Discipline
    DrawCard' _ -> DrawCard
    LongReach' -> LongReach
    Ranged' -> Ranged
    Stupid4' _ -> Stupid4
    Unique' -> Unique

-- | Because this function uses default values (by relying on 'unliftSkill'),
-- it is NOT harmless! Use only when initializing data.
unliftCreature :: Creature UI -> Creature Core
unliftCreature Creature {..} =
  Creature creatureId hp attack (map Card.item items) moral victoryPoints (map unliftSkill skills) ()

unliftCard :: Card UI -> Card Core
unliftCard card =
  case card of
    CreatureCard creature -> CreatureCard $ unliftCreature creature
    NeutralCard n -> NeutralCard $ unliftNeutralObject n
    ItemCard i -> ItemCard $ unliftItemObject i

unliftItemObject :: ItemObject UI -> ItemObject Core
unliftItemObject ItemObject {..} =
  ItemObject {item, itext = (), itextSzOffset = (), itile = (), ititle = (), ititleSzOffset = ()}

unliftNeutralObject :: NeutralObject UI -> NeutralObject Core
unliftNeutralObject NeutralObject {..} =
  NeutralObject {neutral, neutralTeams = (), ntext = (), ntile = (), ntitle = ()}

-- | Because this function uses default values, it is NOT harmless! Use only
-- when initializing data.
unliftSkill :: Skill -> SkillCore
unliftSkill skill =
  case skill of
    Blow -> Blow' True
    Discipline -> Discipline'
    DrawCard -> DrawCard' True
    LongReach -> LongReach'
    Ranged -> Ranged'
    Stupid4 -> Stupid4' 0
    Unique -> Unique'

cardToCreature :: Card p -> Maybe (Creature p)
cardToCreature (CreatureCard creature) = Just creature
cardToCreature (NeutralCard _) = Nothing
cardToCreature (ItemCard _) = Nothing

cardToItemObject :: Card p -> Maybe (ItemObject p)
cardToItemObject (NeutralCard n) = Nothing
cardToItemObject (CreatureCard _) = Nothing
cardToItemObject (ItemCard i) = Just i

cardToNeutralObject :: Card p -> Maybe (NeutralObject p)
cardToNeutralObject (NeutralCard n) = Just n
cardToNeutralObject (CreatureCard _) = Nothing
cardToNeutralObject (ItemCard _) = Nothing

-- | The minimal identifier of a card. See 'SharedModel' to obtain
-- | a full-fledged card from that.
data ID
  = IDC CreatureID
  | IDI Item
  | IDN Neutral
  deriving (Eq, Generic, Ord, Show)

cardToIdentifier :: Card p -> ID
cardToIdentifier card =
  case card of
    CreatureCard Creature {..} -> IDC creatureId
    ItemCard ItemObject {..} -> IDI item
    NeutralCard NeutralObject {..} -> IDN neutral

identToId :: ID -> Maybe CreatureID
identToId (IDC cid) = Just cid
identToId _ = Nothing

groupCards :: [Card p] -> Map.Map ID [Card p]
groupCards xs = Map.fromListWith (++) [(cardToIdentifier x, [x]) | x <- xs]

teamDeck ::
  -- | The cards as loaded from disk
  [Card UI] ->
  -- | The team for which to build the deck
  Team ->
  -- | The initial deck
  [Card Core]
teamDeck cards t =
  map CreatureCard creatures
    ++ map NeutralCard neutrals
    ++ map ItemCard items
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
    items =
      map (\case ItemCard i -> Just i; _ -> Nothing) cards
        & catMaybes
        & map unliftItemObject

data CardTargetKind
  = -- | Card targets empty 'CardSpot'
    Hole
  | -- | Card targets occupied 'CardSpot'
    Occupied
  deriving (Eq, Show)

data TargetType = CardTargetType CardTargetKind | PlayerTargetType
  deriving (Eq, Show)

-- | The kind of Game target a neutral likes
targetType :: Card.ID -> TargetType
targetType id =
  case id of
    IDC _ -> CardTargetType Hole
    IDI _ -> CardTargetType Occupied
    IDN Health -> CardTargetType Occupied
    IDN Life -> CardTargetType Occupied
    IDN InfernalHaste -> PlayerTargetType

-- | The total attack of a creature, including boosts of skills and items.
-- This would make more sense to be in 'Game', but alas this is more
-- convenient to have it here dependency-wise.
totalAttack :: Creature 'Core -> Int
totalAttack Creature {..} =
  attack + (nbAvailBoosts * Constants.boostAmount)
  where
    nbAvailBoosts =
      filter (\case Blow' True -> True; _ -> False) skills & length
