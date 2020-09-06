{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Cinema where

import Card (CreatureID)
import qualified Data.Map.Strict as Map

data State = State
  { -- | Whether the element says something
    telling :: Maybe String,
    -- | 0 means on the left
    x :: Int,
    -- | 0 means top
    y :: Int
  }
  deriving (Show)

data Element
  = -- The actor's unique identifier, and its tile
    Actor Int CreatureID
  | Tile
  deriving (Eq, Ord, Show)

data Scene = Scene
  { -- | The duration of a scene, in tenth of seconds
    duration :: Int,
    -- | The scene's elements
    mapping :: Map.Map Element State
  }
  deriving (Show)

-- | The change to an actor's 'State'
data Change = Change
  { -- | The change to 'telling'
    tellingChange :: Maybe String,
    -- | The change to 'x'
    xoffset :: Int,
    -- | The change to 'y'
    yoffset :: Int
  }
  deriving (Show)

empty :: Scene
empty = Scene {duration = 0, mapping = Map.empty}

at :: Int -> Int -> Change
at x y = Change {tellingChange = Nothing, xoffset = x, yoffset = y}

down :: Change
down = at 0 1

left :: Change
left = at (-1) 0

right :: Change
right = at 1 0

up :: Change
up = at 0 (-1)

initial :: Change -> State
initial Change {..} = State {telling = tellingChange, x = xoffset, y = yoffset}

apply :: Change -> State -> State
apply Change {..} s@State {x, y} =
  s {telling = tellingChange, x = x + xoffset, y = y + yoffset}

-- | Change the state of an existing 'Element' in a 'Scene'
(~->) :: Scene -> (Element, Change) -> Scene
(~->) s@Scene {..} (element, change) =
  case mapping Map.!? element of
    Nothing -> error $ "Element not in scene: " ++ show element
    Just state -> s {mapping = Map.insert element (apply change state) mapping}

-- | Sets the state of a new 'Element' in a 'Scene'
(+->) :: Scene -> (Element, Change) -> Scene
(+->) s@Scene {..} (element, change) =
  case mapping Map.!? element of
    Nothing -> s {mapping = Map.insert element (initial change) mapping}
    Just _ -> error $ "Element in scene already: " ++ show element

-- | Removes an 'Element' from a 'Scene'
(-->) :: Scene -> Element -> Scene
(-->) s@Scene {..} element =
  case mapping Map.!? element of
    Nothing -> error $ "Element not in scene already " ++ show element
    Just _ -> s {mapping = Map.delete element mapping}
