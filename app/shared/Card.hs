{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
import Debug.Trace (traceShow)
import GHC.Generics (Generic)
import Nat
import Skill (Skill)
import qualified Skill
import Tile (Tile)

-- If you add a member, augment the tests in 'Balance.hs'
data Team = Evil | Human | Undead | ZKnights
  deriving (Bounded, Enum, Eq, Generic, Show, Ord)

ppTeam :: Team -> String
ppTeam t =
  case t of
    ZKnights -> "Zealous Knights"
    _ -> show t

allTeams :: [Team]
allTeams = [minBound ..]

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
  SkillType 'Core = Skill.State

type family TeamsType (p :: Phase) where
  TeamsType 'UI = [Team]
  TeamsType 'Core = ()

type family MaybeTextType (p :: Phase) where
  MaybeTextType 'UI = Maybe String
  MaybeTextType 'Core = ()

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
    c (MaybeTextType p),
    c (OffsetType p),
    c (SkillType p),
    c (TeamsType p),
    c (TextType p),
    c (TileType p),
    c (TransientType p)
  )

data CreatureKind
  = Archer
  | Captain
  | Church
  | General
  | Ghost
  | King
  | Knight
  | Necromancer
  | Mummy
  | Priest
  | Skeleton
  | Shade
  | Spearman
  | Specter
  | Squire
  | Swordsman
  | Ogre
  | Vampire
  | Veteran
  | Warrior
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

allCreatureKinds :: [CreatureKind]
allCreatureKinds = [minBound ..]

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

data Neutral
  = Health
  | InfernalHaste
  | Life
  | Plague
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

allNeutrals :: [Neutral]
allNeutrals = [minBound ..]

-- If Creature and NeutralObject start having more in common than solely
-- tile/ntile, a new record can be introduced; to share code.

data NeutralObject (p :: Phase) = NeutralObject
  { neutral :: Neutral,
    -- | The teams to which this neutral card applies
    teams :: TeamsType p,
    title :: TextType p
  }
  deriving (Generic)

deriving instance Forall Eq p => Eq (NeutralObject p)

deriving instance Forall Ord p => Ord (NeutralObject p)

deriving instance Forall Show p => Show (NeutralObject p)

data Item
  = Crown
  | FlailOfTheDamned
  | SkBanner
  | SwordOfMight
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

allItems :: [Item]
allItems = [minBound ..]

data ItemObject (p :: Phase) = ItemObject
  { item :: Item,
    teams :: TeamsType p,
    title :: TextType p,
    titleSzOffset :: OffsetType p
  }
  deriving (Generic)

mkCoreItemObject :: Item -> ItemObject 'Core
mkCoreItemObject item = ItemObject item () () ()

deriving instance Forall Eq p => Eq (ItemObject p)

deriving instance Forall Ord p => Ord (ItemObject p)

deriving instance Forall Show p => Show (ItemObject p)

-- | Data that is used by all three kind of cards
data CardCommon (p :: Phase) = CardCommon
  { mana :: ManaType p,
    text :: MaybeTextType p,
    textSzOffset :: OffsetType p,
    tile :: TileType p
  }
  deriving (Generic)

mkCoreCardCommon :: CardCommon 'Core
mkCoreCardCommon = CardCommon {mana = (), text = (), textSzOffset = (), tile = ()}

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

class Unlift t where
  unlift :: t 'UI -> t 'Core

instance Unlift Creature where
  unlift :: Creature 'UI -> Creature 'Core
  unlift Creature {..} =
    Creature {items = items', skills = skills', ..}
    where
      items' = map Card.item items
      skills' = map Skill.unlift skills
      transient = False

instance Unlift ItemObject where
  unlift ItemObject {..} =
    ItemObject {teams = (), item, title = (), titleSzOffset = ()}

instance Unlift NeutralObject where
  unlift NeutralObject {..} =
    NeutralObject {neutral, teams = (), title = ()}

instance Unlift Card where
  unlift :: Card 'UI -> Card 'Core
  unlift =
    \case
      CreatureCard _ c -> CreatureCard mkCoreCardCommon $ unlift c
      NeutralCard _ n -> NeutralCard mkCoreCardCommon $ unlift n
      ItemCard _ i -> ItemCard mkCoreCardCommon $ unlift i

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

rawTeamDeck ::
  -- | The cards as loaded from disk
  [Card 'UI] ->
  -- | The team for which to build the deck
  Team ->
  -- | The initial deck
  [Maybe (Card 'Core)]
rawTeamDeck cards t =
  map (fmap $ CreatureCard mkCoreCardCommon) creatures
    ++ map (fmap $ NeutralCard mkCoreCardCommon) neutrals
    ++ map (fmap $ ItemCard mkCoreCardCommon) items
  where
    -- Initial creatures:
    creatures =
      case t of
        Evil -> 1 * Knight
        Human -> 3 * Spearman ++ 2 * Archer ++ 1 * General
        Undead -> 2 * Skeleton ++ 2 * Archer ++ 3 * Mummy ++ 1 * Vampire
        ZKnights -> 1 * King ++ 3 * Knight ++ 1 * Captain ++ 1 * Veteran ++ 1 * Priest ++ 2 * Card.Squire
      where
        kindToCreature :: Map.Map CreatureKind (Creature 'Core) =
          map cardToCreature cards
            & catMaybes
            & filter (\c -> (creatureId c & team) == t)
            & map unlift
            & map ((creatureKind . creatureId) &&& id)
            & Map.fromList
        (*) i k = replicate i $ kindToCreature !? k
    -- Initial neutrals
    neutrals =
      case t of
        Evil -> []
        Human -> 1 * Health ++ 1 * Life
        Undead -> 2 * InfernalHaste ++ 1 * Plague
        ZKnights -> 1 * Life
      where
        teams NeutralObject {teams} = teams
        kindToNeutral :: Map.Map Neutral (NeutralObject 'Core) =
          mapMaybe cardToNeutralObject cards
            & filter (\nobj -> t `Prelude.elem` teams nobj)
            & map unlift
            & map (\nobj -> (neutral nobj, nobj))
            & Map.fromList
        (*) i k = replicate i $ kindToNeutral !? k
    -- Initial items
    items =
      case t of
        Evil -> []
        Human -> 1 * SwordOfMight
        Undead -> 1 * Card.FlailOfTheDamned
        ZKnights -> 1 * SwordOfMight
      where
        itemToItemObj :: Map.Map Item (ItemObject 'Core) =
          mapMaybe cardToItemObject cards
            & map (\iobj@ItemObject {item} -> (item, Card.unlift iobj))
            & Map.fromList
        (*) i k = replicate i $ itemToItemObj !? k
    (!?) m k =
      -- To help debug when there's a bug. Will not happen in production.
      case m Map.!? k of
        Nothing -> traceShow ("Key not found: " ++ show k) Nothing
        res@(Just _) -> res

teamDeck ::
  -- | The cards as loaded from disk
  [Card 'UI] ->
  -- | The team for which to build the deck
  Team ->
  -- | The initial deck
  [(Card 'Core)]
teamDeck cards t = rawTeamDeck cards t & catMaybes

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
