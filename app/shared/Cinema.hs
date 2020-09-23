{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Module containing the base for animating scenes in lobbies
module Cinema
  ( Change (Leave),
    Direction (..),
    DirectionChange,
    Element (..),
    MappingType (..),
    Phase (..),
    Scene (..),
    Shooting (..),
    State (..),
    StayChange,
    TellChange,
    (=:),
    at,
    at',
    defaultDirection,
    display,
    down,
    initial,
    left,
    mkChange,
    patch,
    patch',
    right,
    shoot,
    shutup,
    tell,
    up,
    while,
  )
where

import Card (CreatureID)
import Data.Function ((&))
import Data.Kind (Constraint, Type)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import SharedModel (SharedModel (..))
import Tile (Tile)

data Direction = ToLeft | ToRight -- Suffix with 'To' to avoid clashing with Either
  deriving (Eq, Generic, Ord, Show)

defaultDirection :: Direction
defaultDirection = ToLeft -- How sprite are in oryx' set

data State = State
  { -- | In which direction the sprite is looking at
    direction :: Direction,
    -- | Whether the element says something
    telling :: Maybe String,
    -- | 0 means on the left
    x :: Int,
    -- | 0 means top
    y :: Int
  }
  deriving (Eq, Generic, Ord, Show)

data Element
  = -- The actor's unique identifier, and its tile
    Actor Int CreatureID
  | TileElement Tile
  deriving (Eq, Generic, Ord, Show)

data Phase = Diff | Display

type family MappingValueType (p :: Phase) where
  MappingValueType Diff = Change
  MappingValueType Display = State

type Forall (c :: Type -> Constraint) (p :: Phase) =
  ( c (MappingValueType p)
  )

newtype MappingType p = MappingType {unMappingType :: Map.Map Element (MappingValueType p)}
  deriving (Generic)

deriving instance Forall Eq p => Eq (MappingType p)

deriving instance Forall Ord p => Ord (MappingType p)

deriving instance Forall Show p => Show (MappingType p)

data Scene (p :: Phase) = Scene
  { -- | The duration of a scene, in tenth of seconds
    duration :: Int,
    -- | The scene's elements
    mapping :: MappingType p
  }
  deriving (Generic)

deriving instance Forall Eq p => Eq (Scene p)

deriving instance Forall Ord p => Ord (Scene p)

deriving instance Forall Show p => Show (Scene p)

data DirectionChange = TurnRight | TurnLeft | NoDirectionChange
  deriving (Eq, Generic, Ord, Show)

instance Semigroup DirectionChange where
  TurnLeft <> TurnRight = NoDirectionChange
  TurnLeft <> _ = TurnLeft
  TurnRight <> TurnLeft = NoDirectionChange
  TurnRight <> _ = TurnRight
  NoDirectionChange <> change = change

instance Monoid DirectionChange where
  mempty = NoDirectionChange

data TellChange = Tell String | ShutUp | NoTellChange
  deriving (Eq, Generic, Ord, Show)

-- last change wins
instance Semigroup TellChange where
  change <> NoTellChange = change
  _ <> change = change

instance Monoid TellChange where
  mempty = NoTellChange

-- | The change to an actor's 'State'
data StayChange = StayChange
  { -- | The change to 'direction'
    turn :: DirectionChange,
    -- | The change to 'telling'
    tellingChange :: TellChange,
    -- | The change to 'x'
    xoffset :: Int,
    -- | The change to 'y'
    yoffset :: Int
  }
  deriving (Eq, Generic, Ord, Show)

instance Semigroup StayChange where
  (StayChange turn1 tell1 dx1 dy1) <> (StayChange turn2 tell2 dx2 dy2) =
    StayChange (turn1 <> turn2) (tell1 <> tell2) (dx1 + dx2) (dy1 + dy2)

instance Monoid StayChange where
  mempty = StayChange mempty mempty 0 0

data Change
  = -- | Actor leaves the stage
    Leave
  | -- | Actor stays on stage, but moves or says something
    Stay StayChange
  deriving (Eq, Generic, Ord, Show)

instance Semigroup Change where
  _ <> change = change

instance Monoid Change where
  mempty = Stay mempty

mkChange :: DirectionChange -> TellChange -> Int -> Int -> Change
mkChange turn tellingChange xoffset yoffset = Stay StayChange {..}

at :: Int -> Int -> Change
at x y = Stay StayChange {turn = mempty, tellingChange = mempty, xoffset = x, yoffset = y}

at' :: Direction -> Int -> Int -> Change
at' dir x y = Stay StayChange {turn = turnFrom dir, tellingChange = mempty, xoffset = x, yoffset = y}
  where
    turnFrom ToRight = TurnRight
    turnFrom ToLeft = TurnLeft

down :: Change
down = at 0 1

left :: Change
left = at (-1) 0

right :: Change
right = at 1 0

shutup :: Change
shutup = Stay StayChange {turn = mempty, tellingChange = ShutUp, xoffset = 0, yoffset = 0}

tell :: String -> Change
tell s = Stay StayChange {turn = mempty, tellingChange = Tell s, xoffset = 0, yoffset = 0}

up :: Change
up = at 0 (-1)

initial :: Scene Diff -> Scene Display
initial Scene {duration, mapping = MappingType changes} =
  Scene {..}
  where
    mapping = Map.map initial' changes & Map.mapMaybe id & MappingType

initial' :: Change -> Maybe State
initial' (Stay StayChange {..}) =
  Just $
    State
      { direction = fromDirectionChange turn,
        telling = fromTellChange tellingChange,
        x = xoffset,
        y = yoffset
      }
  where
    fromDirectionChange TurnLeft = ToLeft
    fromDirectionChange TurnRight = ToRight
    fromDirectionChange NoDirectionChange = defaultDirection
    fromTellChange (Tell s) = Just s
    fromTellChange _ = Nothing
initial' Leave = Nothing

patch :: Scene Display -> Scene Diff -> Scene Display
patch
  Scene {mapping = MappingType prev}
  Scene {duration, mapping = MappingType diff} =
    -- Take duration from Diff: ignore old duration
    Scene {mapping = MappingType mapping'', ..}
    where
      -- Compute elements that make it to the next scene
      elements = Map.keys prev ++ Map.keys diff & Set.fromList
      mapping' = Set.map buildState elements & Set.toList & Map.fromList
      buildState e =
        ( e,
          case (prev Map.!? e, diff Map.!? e) of
            (_, Just Leave) -> Nothing
            (Nothing, Just c) -> initial' c -- No previous state, consider diff as absolute
            (Just p, Just c) -> patch' p c -- Apply diff
            (p, Nothing) -> p -- No diff: keep previous state
        )
      mapping'' = Map.mapMaybe id mapping'

patch' :: State -> Change -> Maybe State
patch' s@State {..} (Stay StayChange {..}) =
  Just $
    s
      { direction = applyDirectionChange direction turn,
        telling = applyTellingChange telling tellingChange,
        x = x + xoffset,
        y = y + yoffset
      }
  where
    applyDirectionChange dir NoDirectionChange = dir
    applyDirectionChange _ TurnRight = ToRight
    applyDirectionChange _ TurnLeft = ToLeft
    applyTellingChange telling NoTellChange = telling
    applyTellingChange _ (Tell s) = Just s
    applyTellingChange _ ShutUp = Nothing
patch' _ Leave = Nothing

(=:) :: Element -> Change -> MappingType Diff
k =: v = MappingType (Map.singleton k v)

instance Semigroup (MappingType Diff) where
  (MappingType m1) <> (MappingType m2) = MappingType (Map.unionWith (<>) m1 m2)

-- | Given a duration and a mapping, builds a 'Scene'
while :: Int -> MappingType p -> Scene p
while i m = Scene {duration = i, mapping = m}

-- | Builds a list of displays from a list of diffs. Interprets the
-- | first diff as an absolute scene (i.e. not as a diff).
display :: [Scene Diff] -> [Scene Display]
display [] = []
display (absolute : diffs) =
  firstDisplay : display' firstDisplay diffs
  where
    firstDisplay = initial absolute
    display' _ [] = []
    display' display (firstDiff : nextDiffs) =
      let nextDisplay = patch display firstDiff
       in nextDisplay : display' nextDisplay nextDiffs

data Shooting = Shooting
  { scene :: Maybe (Scene Display),
    rest :: [Scene Diff]
  }

shoot ::
  SharedModel ->
  -- | The previous scene that was returned, or Nothing if the first call
  Maybe (Scene Display) ->
  -- | The remaing scenes to apply
  [Scene Diff] ->
  Shooting
shoot _ _ [] = Shooting {scene = Nothing, rest = []}
shoot shared prev (diff : rest) =
  let scene = Just $
        case prev of
          -- Building the first scene, 'diff' gets interpreted
          -- as absolute, not a diff.
          Nothing -> initial diff
          -- Patching previous scene
          Just prev -> patch prev diff
   in Shooting {..}
