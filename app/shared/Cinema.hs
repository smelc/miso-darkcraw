{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Module containing the base for animating scenes in lobbies
module Cinema
  ( Change,
    Element (..),
    Phase (..),
    Scene (..),
    State (..),
    (=:),
    (<~>),
    at,
    diff,
    display,
    down,
    initial,
    left,
    right,
    tell,
    up,
    while,
  )
where

import Card (CreatureID)
import Data.Kind (Constraint, Type)
import qualified Data.Map.Strict as Map

data State = State
  { -- | Whether the element says something
    telling :: Maybe String,
    -- | 0 means on the left
    x :: Int,
    -- | 0 means top
    y :: Int
  }
  deriving (Eq, Ord, Show)

data Element
  = -- The actor's unique identifier, and its tile
    Actor Int CreatureID
  | Tile
  deriving (Eq, Ord, Show)

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

deriving instance Forall Show p => Show (Scene p)

-- | The change to an actor's 'State'
data Change = Change
  { -- | The change to 'telling'
    tellingChange :: Maybe String,
    -- | The change to 'x'
    xoffset :: Int,
    -- | The change to 'y'
    yoffset :: Int
  }
  deriving (Eq, Ord, Show)

instance Semigroup Change where
  Change {tellingChange = tell1, xoffset = x1, yoffset = y1}
    <> Change {tellingChange = tell2, xoffset = x2, yoffset = y2} =
      case (tell1, tell2) of
        (Just s1, Just s2) | s1 /= s2 -> error $ "Cannot union tellingChanges: " ++ s1 ++ " VS " ++ s2
        (x, _) -> Change {tellingChange = x, xoffset = x1 + x2, yoffset = y1 + y2}

instance Monoid Change where
  mempty = Change {tellingChange = Nothing, xoffset = 0, yoffset = 0}

at :: Int -> Int -> Change
at x y = Change {tellingChange = Nothing, xoffset = x, yoffset = y}

down :: Change
down = at 0 1

left :: Change
left = at (-1) 0

right :: Change
right = at 1 0

tell :: String -> Change
tell s = Change {tellingChange = Just s, xoffset = 0, yoffset = 0}

up :: Change
up = at 0 (-1)

initial :: Scene Diff -> Scene Display
initial Scene {duration, mapping = changes} =
  Scene {..}
  where
    mapping = Map.map initial' changes

initial' :: Change -> State
initial' Change {..} = State {telling = tellingChange, x = xoffset, y = yoffset}

diff :: Scene Display -> Scene Diff -> Scene Display
diff
  Scene {mapping = prev}
  Scene {duration, mapping = diff} =
    -- Take duration from Diff: ignore old duration
    Scene {..}
    where
      mapping = undefined

diff' :: Change -> State -> State
diff' Change {..} s@State {x, y} =
  s {telling = tellingChange, x = x + xoffset, y = y + yoffset}

(=:) :: Element -> Change -> MappingType Diff
(=:) = Map.singleton

infixr 6 <~>

(<~>) :: MappingType Diff -> MappingType Diff -> MappingType Diff
(<~>) = Map.unionWith (<>)

-- | Given a duration and a mapping, builds a 'Scene'
while :: Int -> MappingType p -> Scene p
while i m = Scene {duration = i, mapping = m}

-- | Builds a list of displays from a list of diffs. Interprets the
-- | first diff as an absolute scene (i.e. not as a diff).
display :: [Scene Diff] -> [Scene Display]
display [] = []
display (absolute : diffs) =
  display' (initial absolute) diffs
  where
    display' _ [] = []
    display' display (firstDiff : nextDiffs) =
      let nextDisplay = diff display firstDiff
       in display : nextDisplay : display' nextDisplay nextDiffs
