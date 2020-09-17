{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Module containing the base for animating scenes in lobbies
module Cinema
  ( Change (Leave),
    Direction (..),
    Element (..),
    Phase (..),
    Scene (..),
    Shooting (..),
    State (..),
    StayChange,
    (=:),
    (<~>),
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
import GHC.Generics
import SharedModel (SharedModel (..))

data Direction = ToLeft | ToRight -- Suffix with 'To' to avoid clasing with Either
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
  | Tile
  deriving (Eq, Generic, Ord, Show)

type Changes = Map.Map Element Change

data Phase = Diff | Display

type family MappingValueType (p :: Phase) where
  MappingValueType Diff = Change
  MappingValueType Display = State

type Forall (c :: Type -> Constraint) (p :: Phase) =
  ( c (MappingValueType p)
  )

type MappingType p = Map.Map Element (MappingValueType p)

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

-- | The change to an actor's 'State'
data StayChange = StayChange
  { -- | The change to 'direction'
    turn :: Maybe Direction,
    -- | The change to 'telling'
    tellingChange :: Maybe String,
    -- | The change to 'x'
    xoffset :: Int,
    -- | The change to 'y'
    yoffset :: Int
  }
  deriving (Eq, Generic, Ord, Show)

data Change
  = -- | Actor leaves the stage
    Leave
  | -- | Actor stays on stage, but moves or says something
    Stay StayChange
  deriving (Eq, Generic, Ord, Show)

mkChange :: Maybe Direction -> Maybe String -> Int -> Int -> Change
mkChange turn tellingChange xoffset yoffset = Stay StayChange {..}

at :: Int -> Int -> Change
at x y = Stay StayChange {turn = Nothing, tellingChange = Nothing, xoffset = x, yoffset = y}

at' :: Direction -> Int -> Int -> Change
at' dir x y = Stay StayChange {turn = Just dir, tellingChange = Nothing, xoffset = x, yoffset = y}

down :: Change
down = at 0 1

left :: Change
left = at (-1) 0

right :: Change
right = at 1 0

shutup :: Change
shutup = Stay StayChange {turn = Nothing, tellingChange = Nothing, xoffset = 0, yoffset = 0}

tell :: String -> Change
tell s = Stay StayChange {turn = Nothing, tellingChange = Just s, xoffset = 0, yoffset = 0}

up :: Change
up = at 0 (-1)

initial :: Scene Diff -> Scene Display
initial Scene {duration, mapping = changes} =
  Scene {..}
  where
    mapping = Map.map initial' changes & Map.mapMaybe id

initial' :: Change -> Maybe State
initial' (Stay StayChange {..}) =
  Just $
    State
      { direction = fromMaybe defaultDirection turn,
        telling = tellingChange,
        x = xoffset,
        y = yoffset
      }
initial' Leave = Nothing

patch :: Scene Display -> Scene Diff -> Scene Display
patch
  Scene {mapping = prev}
  Scene {duration, mapping = diff} =
    -- Take duration from Diff: ignore old duration
    Scene {mapping = mapping'', ..}
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
patch' s@State {x, y} (Stay StayChange {..}) =
  Just $ s {telling = tellingChange, x = x + xoffset, y = y + yoffset}
patch' _ Leave = Nothing

(=:) :: Element -> Change -> MappingType Diff
(=:) = Map.singleton

infixr 6 <~>

(<~>) :: MappingType Diff -> MappingType Diff -> MappingType Diff
(<~>) = Map.unionWithKey checkDisjoint
  where
    checkDisjoint k a b = error $ "Duplicate diff for element: " ++ show k

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
