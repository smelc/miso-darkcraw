{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Nat
import Tile

-- If you change the first member, change 'allTeams' too
-- If you add a member, augment the tests in 'Balance.hs'
data Team = Evil | Human | Undead
  deriving (Enum, Eq, Generic, Show, Ord)

ppTeam :: Team -> String
ppTeam = show

allTeams :: [Team]
allTeams = [Evil ..]

data Skill
  = Blow
  | BreathIce
  | Discipline
  | DrawCard
  | -- | Creature causes fear
    Fear
  | LongReach
  | Ranged
  | -- | Creature creates mana at beginning of turn
    Source Nat
  | Stupid4
  | -- | Creature causes terror
    Terror
  | Unique
  deriving (Eq, Generic, Ord, Show)

data SkillCore
  = -- | Whether the skill is available (True) or used already (False)
    Blow' Bool
  | BreathIce'
  | Discipline'
  | -- | Whether the skill is available (True) or used already (False)
    DrawCard' Bool
  | -- | Creature causes fear, the Boolean indicates whether the skill
    -- is available (True) or used already (False)
    Fear' Bool
  | LongReach'
  | Ranged'
  | -- | Creature create mana at beginning of turn. Boolean indicates
    -- whether the skill is available (True) or used already (False)
    Source' Nat Bool
  | -- | The turn, at 0, 1, or 2 not stupide; at 3 stupid; then back to 0
    Stupid4' Int
  | -- | Creature causes terror, the Boolean indicates whether the skill
    -- is available (True) or used already (False)
    Terror' Bool
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
  ItemType 'UI = ItemObject 'UI
  ItemType 'Core = Item

type family ManaType (p :: Phase) where
  ManaType 'UI = Nat -- Mana cost
  ManaType 'Core = () -- Card is on table, mana doesn't matter

type family OffsetType (p :: Phase) where
  OffsetType 'UI = Int
  OffsetType 'Core = ()

type family SkillType (p :: Phase) where
  SkillType 'UI = Skill
  SkillType 'Core = SkillCore

type family TeamsType (p :: Phase) where
  TeamsType 'UI = [Team]
  TeamsType 'Core = ()

type family TextType (p :: Phase) where
  TextType 'UI = String
  TextType 'Core = ()

type family TileType (p :: Phase) where
  TileType 'UI = Tile
  TileType 'Core = ()

type family TransientType (p :: Phase) where
  TransientType 'UI = ()
  TransientType 'Core = Bool

type Forall (c :: Type -> Constraint) (p :: Phase) =
  ( c (ItemType p),
    c (ManaType p),
    c (OffsetType p),
    c (SkillType p),
    c (TeamsType p),
    c (TextType p),
    c (TileType p),
    c (TransientType p)
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
  | Specter
  | Swordsman
  | Ogre
  | Vampire
  | Warrior
  deriving (Enum, Eq, Generic, Ord, Show)

allCreatureKinds :: [CreatureKind]
allCreatureKinds = [Archer ..]

data CreatureID = CreatureID {creatureKind :: CreatureKind, team :: Team}
  deriving (Eq, Generic, Ord, Show)

isSkeleton :: CreatureID -> Bool
isSkeleton CreatureID {creatureKind = kind, team} =
  case (kind, team) of
    (Archer, Undead) -> True
    (Skeleton, Undead) -> True
    (Warrior, Undead) -> True
    _ -> False

data Creature (p :: Phase) = Creature
  { creatureId :: CreatureID,
    hp :: Nat,
    -- | Beware when using this accessor, you may want 'totalAttack' instead
    attack :: Nat,
    items :: [ItemType p],
    moral :: Int,
    skills :: [SkillType p],
    -- | Creature doesn't go to the discarded stack when killed
    transient :: TransientType p
  }
  deriving (Generic)

deriving instance Forall Eq p => Eq (Creature p)

deriving instance Forall Ord p => Ord (Creature p)

deriving instance Forall Show p => Show (Creature p)

class Itemizable a where
  getItems :: a -> [Item]

instance Itemizable (Creature 'UI) where
  getItems Creature {items} = map item items

instance Itemizable (Creature 'Core) where
  getItems Creature {items} = items

-- If you change the first member, change 'allNeutrals' too
data Neutral
  = Health
  | InfernalHaste
  | Life
  | Plague
  deriving (Enum, Eq, Generic, Ord, Show)

allNeutrals :: [Neutral]
allNeutrals = [Health ..]

-- If Creature and NeutralObject start having more in common than solely
-- tile/ntile, a new record can be introduced; to share code.

data NeutralObject (p :: Phase) = NeutralObject
  { neutral :: Neutral,
    -- | The teams to which this neutral card applies
    neutralTeams :: TeamsType p,
    ntext :: TextType p,
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
  | SkBanner
  | SwordOfMight
  deriving (Enum, Eq, Generic, Ord, Show)

allItems :: [Item]
allItems = [Card.Crown ..]

data ItemObject (p :: Phase) = ItemObject
  { item :: Item,
    teams :: TeamsType p,
    itext :: TextType p,
    itextSzOffset :: OffsetType p,
    ititle :: TextType p,
    ititleSzOffset :: OffsetType p
  }
  deriving (Generic)

mkCoreItemObject :: Item -> ItemObject 'Core
mkCoreItemObject item = ItemObject item () () () () ()

deriving instance Forall Eq p => Eq (ItemObject p)

deriving instance Forall Ord p => Ord (ItemObject p)

deriving instance Forall Show p => Show (ItemObject p)

-- | Data that is used by all three kind of cards
data CardCommon (p :: Phase) = CardCommon
  { mana :: ManaType p,
    tile :: TileType p
  }
  deriving (Generic)

mkCoreCardCommon :: CardCommon 'Core
mkCoreCardCommon = CardCommon {mana = (), tile = ()}

deriving instance Forall Eq p => Eq (CardCommon p)

deriving instance Forall Ord p => Ord (CardCommon p)

deriving instance Forall Show p => Show (CardCommon p)

-- TODO: Get rid of the duplication of the first parameter by introducing
-- an extra level?
data Card (p :: Phase)
  = CreatureCard (CardCommon p) (Creature p)
  | NeutralCard (CardCommon p) (NeutralObject p)
  | ItemCard (CardCommon p) (ItemObject p)

deriving instance Forall Eq p => Eq (Card p)

deriving instance Forall Ord p => Ord (Card p)

deriving instance Forall Show p => Show (Card p)

deriving instance Generic (Card p)

toCommon :: Card p -> CardCommon p
toCommon (CreatureCard common _) = common
toCommon (NeutralCard common _) = common
toCommon (ItemCard common _) = common

liftSkill :: SkillCore -> Skill
liftSkill skill =
  case skill of
    Blow' _ -> Blow
    BreathIce' -> BreathIce
    Discipline' -> Discipline
    DrawCard' _ -> DrawCard
    Fear' _ -> Fear
    LongReach' -> LongReach
    Ranged' -> Ranged
    Source' n _ -> Source n
    Stupid4' _ -> Stupid4
    Terror' _ -> Terror
    Unique' -> Unique

class Unlift t where
  unlift :: t 'UI -> t 'Core

instance Unlift Creature where
  unlift :: Creature 'UI -> Creature 'Core
  unlift Creature {..} =
    Creature {items = items', skills = skills', ..}
    where
      items' = map Card.item items
      skills' = map unliftSkill skills
      transient = False

instance Unlift ItemObject where
  unlift ItemObject {..} =
    ItemObject {teams = (), item, itext = (), itextSzOffset = (), ititle = (), ititleSzOffset = ()}

instance Unlift NeutralObject where
  unlift NeutralObject {..} =
    NeutralObject {neutral, neutralTeams = (), ntext = (), ntitle = ()}

instance Unlift Card where
  unlift :: Card 'UI -> Card 'Core
  unlift =
    \case
      CreatureCard _ c -> CreatureCard mkCoreCardCommon $ unlift c
      NeutralCard _ n -> NeutralCard mkCoreCardCommon $ unlift n
      ItemCard _ i -> ItemCard mkCoreCardCommon $ unlift i

-- | Because this function uses default values, it is NOT harmless! Use only
-- when initializing data.
unliftSkill :: Skill -> SkillCore
unliftSkill skill =
  case skill of
    Blow -> Blow' True
    BreathIce -> BreathIce'
    Discipline -> Discipline'
    DrawCard -> DrawCard' True
    Fear -> Fear' True
    LongReach -> LongReach'
    Ranged -> Ranged'
    Source n -> Source' n True
    Stupid4 -> Stupid4' 0
    Terror -> Terror' True
    Unique -> Unique'

cardToCreature :: Card p -> Maybe (Creature p)
cardToCreature (CreatureCard _ creature) = Just creature
cardToCreature (NeutralCard {}) = Nothing
cardToCreature (ItemCard {}) = Nothing

cardToItemObject :: Card p -> Maybe (ItemObject p)
cardToItemObject (NeutralCard {}) = Nothing
cardToItemObject (CreatureCard {}) = Nothing
cardToItemObject (ItemCard _ i) = Just i

cardToNeutralObject :: Card p -> Maybe (NeutralObject p)
cardToNeutralObject (NeutralCard _ n) = Just n
cardToNeutralObject (CreatureCard {}) = Nothing
cardToNeutralObject (ItemCard {}) = Nothing

-- | The minimal identifier of a card. See 'SharedModel' to obtain
-- | a full-fledged card from that.
data ID
  = IDC CreatureID [Item]
  | IDI Item
  | IDN Neutral
  deriving (Eq, Generic, Ord, Show)

cardToIdentifier :: Itemizable (Creature p) => Card p -> ID
cardToIdentifier =
  \case
    CreatureCard _ c@Creature {creatureId} -> IDC creatureId $ getItems c
    ItemCard _ ItemObject {item} -> IDI item
    NeutralCard _ NeutralObject {neutral} -> IDN neutral

identToId :: ID -> Maybe CreatureID
identToId (IDC cid _) = Just cid
identToId _ = Nothing

groupCards :: Itemizable (Creature p) => [Card p] -> Map.Map ID [Card p]
groupCards xs = Map.fromListWith (++) [(cardToIdentifier x, [x]) | x <- xs]

teamDeck ::
  -- | The cards as loaded from disk
  [Card 'UI] ->
  -- | The team for which to build the deck
  Team ->
  -- | The initial deck
  [Card 'Core]
teamDeck cards t =
  map (CreatureCard mkCoreCardCommon) creatures
    ++ map (NeutralCard mkCoreCardCommon) neutrals
    ++ map (ItemCard mkCoreCardCommon) items
  where
    -- Initial creatures:
    creatures =
      case t of
        Evil -> 1 * Knight
        Human -> 3 * Spearman ++ 2 * Archer ++ 1 * General
        Undead -> 2 * Skeleton ++ 2 * Archer ++ 3 * Mummy ++ 1 * Vampire
      where
        kindToCreature :: Map.Map CreatureKind (Creature 'Core) =
          map cardToCreature cards
            & catMaybes
            & filter (\c -> (creatureId c & team) == t)
            & map unlift
            & map ((creatureKind . creatureId) &&& id)
            & Map.fromList
        (*) i k = replicate i $ kindToCreature Map.! k
    -- Initial neutrals
    neutrals =
      case t of
        Evil -> []
        Human -> 1 * Health ++ 1 * Life
        Undead -> 2 * InfernalHaste ++ 1 * Plague
      where
        kindToNeutral :: Map.Map Neutral (NeutralObject 'Core) =
          mapMaybe cardToNeutralObject cards
            & filter (\nobj -> t `Prelude.elem` neutralTeams nobj)
            & map unlift
            & map (\nobj -> (neutral nobj, nobj))
            & Map.fromList
        (*) i k = replicate i $ kindToNeutral Map.! k
    -- Initial items
    items =
      case t of
        Evil -> []
        Human -> 1 * Card.Crown ++ 1 * SwordOfMight
        Undead -> 1 * Card.FlailOfTheDamned
      where
        itemToItemObj :: Map.Map Item (ItemObject 'Core) =
          mapMaybe cardToItemObject cards
            & map (\iobj@ItemObject {item} -> (item, Card.unlift iobj))
            & Map.fromList
        (*) i k = replicate i $ itemToItemObj Map.! k

data CardTargetKind
  = -- | Card targets empty 'CardSpot'
    Hole
  | -- | Card targets occupied 'CardSpot'
    Occupied
  deriving (Eq, Show)

data TargetType
  = -- | Target is a single card
    CardTargetType CardTargetKind
  | -- | Target is an entire part of the bard
    PlayerTargetType
  deriving (Eq, Show)

-- | The kind of Game target a neutral likes
targetType :: Card.ID -> TargetType
targetType id =
  case id of
    IDC {} -> CardTargetType Hole
    IDI _ -> CardTargetType Occupied
    IDN Health -> CardTargetType Occupied
    IDN Life -> CardTargetType Occupied
    IDN InfernalHaste -> PlayerTargetType
    IDN Plague -> PlayerTargetType
