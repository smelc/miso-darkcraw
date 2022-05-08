{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Card where

import Control.Arrow ((&&&))
import Damage (Damage, Dealer (..))
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Kind (Constraint, Type)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Debug.Trace (traceShow)
import GHC.Generics (Generic)
import qualified Mana
import Nat
import Skill (Skill)
import qualified Skill
import Tile (Tile)

-- If you add a member, augment the tests in 'Balance.hs'
data Team = Beastmen | Evil | Human | Sylvan | Undead | ZKnights
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
  ManaType 'UI = Mana.Mana -- Mana cost
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

-- | All kinds of creature
data CreatureKind
  = Abomination
  | Archer
  | Assassin
  | Bear
  | Beholder
  | Bird
  | Captain
  | Church
  | Daemon
  | Defender
  | Falcon
  | Falconer
  | General
  | Ghost
  | King
  | Knight
  | Necromancer
  | Minotaur
  | Mummy
  | Ogre
  | Priest
  | Skeleton
  | Shade
  | Spearman
  | Specter
  | Squire
  | Swordsman
  | Trebuchet
  | Tree
  | Troll
  | Vampire
  | Veteran
  | Warrior
  | Worm
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

allCreatureKinds :: [CreatureKind]
allCreatureKinds = [minBound ..]

-- | The identifier of a creature. Not all identifiers are actually mapped
-- by 'Shared.Model'.
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
    attack :: Damage,
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

instance Dealer (Creature p) where
  dealer Creature {attack} = dealer attack

class Itemizable a where
  getItems :: a -> [Item]

instance Itemizable (Creature 'UI) where
  getItems Creature {items} = map item items

instance Itemizable (Creature 'Core) where
  getItems Creature {items} = items

data Neutral
  = Health
  | HuntingHorn
  | InfernalHaste
  | Life
  | Pandemonium
  | Plague
  | StrengthPot
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

allNeutrals :: [Neutral]
allNeutrals = [minBound ..]

data NeutralObject (p :: Phase) = NeutralObject
  { neutral :: Neutral,
    -- | The teams to which this neutral card applies
    teams :: TeamsType p,
    title :: TextType p,
    titleSzOffset :: OffsetType p
  }
  deriving (Generic)

deriving instance Forall Eq p => Eq (NeutralObject p)

deriving instance Forall Ord p => Ord (NeutralObject p)

deriving instance Forall Show p => Show (NeutralObject p)

data Item
  = AxeOfRage
  | BannerFeather
  | BowOfGaia
  | BowOfStrength
  | CloakOfGaia
  | Crown
  | CrushingMace
  | FlailOfTheDamned
  | SkBanner
  | SpikyMace
  | SwordOfMight
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

-- | Requirements for being able to have some item
data Requirement
  = -- | No requirement
    NoReq
  | -- | Requirement to have some skill
    SomeReq Skill.Skill

-- | Requirements for being able to carry an item. Could be encoded
-- in json, but I'm afraid complex requirements would be tartelette to implement.
requirement :: Item -> Requirement
requirement = \case
  AxeOfRage -> NoReq
  BannerFeather -> NoReq
  BowOfGaia -> NoReq
  BowOfStrength -> SomeReq Skill.Ace
  CloakOfGaia -> NoReq
  Crown -> NoReq
  CrushingMace -> NoReq
  FlailOfTheDamned -> NoReq
  SkBanner -> NoReq
  SpikyMace -> NoReq
  SwordOfMight -> NoReq

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
    NeutralObject {neutral, teams = (), title = (), titleSzOffset = ()}

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

-- | The minimal identifier of a card. See 'Shared.Model' to obtain
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
        Beastmen -> 2 * Defender ++ 1 * Minotaur
        Evil -> 2 * Knight ++ 2 * Spearman ++ 1 * Daemon ++ 1 * Beholder ++ 1 * Abomination ++ 1 * Priest ++ 1 * Assassin
        Human -> 3 * Spearman ++ 2 * Archer ++ 1 * General
        Sylvan -> 3 * Archer ++ 2 * Tree ++ 2 * Priest ++ 2 * Bear ++ 1 * Falconer
        Undead -> 2 * Skeleton ++ 2 * Archer ++ 3 * Mummy ++ 1 * Vampire
        ZKnights -> 1 * King ++ 3 * Knight ++ 1 * Captain ++ 1 * Veteran ++ 1 * Priest ++ 2 * Card.Squire ++ 2 * Card.Trebuchet ++ 1 * Card.Bird
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
        Beastmen -> []
        Evil -> 1 * Pandemonium ++ 1 * StrengthPot
        Human -> 1 * Health ++ 1 * Life
        Sylvan -> 1 * Life
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
        Beastmen -> []
        Evil -> 1 * SpikyMace
        Human -> 1 * SwordOfMight
        Sylvan -> 1 * CloakOfGaia
        Undead -> 1 * Card.FlailOfTheDamned
        ZKnights -> 1 * CrushingMace ++ 1 * SwordOfMight
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

-- | The kind of 'TargetType' that a 'Card.ID' aims
targetType :: Card.ID -> TargetType
targetType id =
  case id of
    IDC {} -> CardTargetType Hole
    IDI _ -> CardTargetType Occupied
    IDN Health -> CardTargetType Occupied
    IDN HuntingHorn -> PlayerTargetType
    IDN Life -> CardTargetType Occupied
    IDN InfernalHaste -> PlayerTargetType
    IDN Pandemonium -> PlayerTargetType
    IDN Plague -> PlayerTargetType
    IDN StrengthPot -> CardTargetType Occupied

-- * Classes and instances

-- Classes and instances that are meant to be used qualified
-- (and hence are not in 'CardInstances'). These classes are mostly
-- about making code shorter and avoid repeating default values or patterns.
-- They do not do anything smart.

-- | Class for runtime values having some quality. This is akin to
-- a strongly typed entity system.
class Has a b where
  has :: a -> b -> Bool
  doesNotHave :: a -> b -> Bool
  doesNotHave a b = not $ has a b

instance Has (Creature 'Core) Skill.State where
  has Creature {skills} skill = skill `elem` skills

instance Has (Creature 'Core) Item where
  has Creature {items} item = item `elem` items

instance Has a b => Has (Maybe a) b where
  has a b =
    case a of
      Nothing -> False
      Just a -> a `has` b

-- | Class from which some value can be obtained
class To a b where
  to :: a -> b

instance To (Creature 'Core) [Skill.State] where
  to Creature {skills} = skills

instance (Monoid b, To a b) => To (Maybe a) b where
  to Nothing = mempty
  to (Just a) = to a
