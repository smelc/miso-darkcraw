{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Module containing the base for animating scenes in lobbies
module Cinema
  ( ActorChange (Leave),
    Direction (..),
    DirectionChange,
    Element (..),
    Frame (..),
    ActorState (..),
    Scene (..),
    StayChange,
    TellingChange,
    TimedFrame (..),
    (=:),
    (|||),
    at,
    at',
    defaultDirection,
    display,
    down,
    initial,
    left,
    mkChange,
    patch,
    patchActorState,
    right,
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
import qualified Data.Map.Merge.Strict as Map
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

data ActorState = ActorState
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

defaultActorState :: ActorState
defaultActorState =
  ActorState
    { direction = defaultDirection,
      telling = Nothing,
      x = 0,
      y = 0
    }

data Element
  = -- The actor's unique identifier, and its tile
    Actor Int CreatureID
  | TileElement Tile
  deriving (Eq, Generic, Ord, Show)

newtype Frame a = Frame {unFrame :: Map.Map Element a}
  deriving (Eq, Ord, Show, Generic)

data TimedFrame a = TimedFrame
  { -- | The duration of a frame, in tenth of seconds
    duration :: Int,
    -- | The frame
    frame :: Frame a
  }
  deriving (Eq, Ord, Show, Generic)

newtype Scene a = Scene {frames :: [TimedFrame a]}
  deriving (Eq, Ord, Show, Generic)

instance Semigroup (Scene a) where
  (Scene frames1) <> (Scene frames2) = Scene (frames1 <> frames2)

instance Monoid (Scene a) where
  mempty = Scene []

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

data TellingChange = Tell String | ShutUp | NoTellingChange
  deriving (Eq, Generic, Ord, Show)

-- last change wins
instance Semigroup TellingChange where
  change <> NoTellingChange = change
  _ <> change = change

instance Monoid TellingChange where
  mempty = NoTellingChange

-- | The change to an actor's 'ActorState'
data StayChange = StayChange
  { -- | The change to 'direction'
    turn :: DirectionChange,
    -- | The change to 'telling'
    tellingChange :: TellingChange,
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

data ActorChange
  = -- | Actor leaves the stage
    Leave
  | -- | Actor stays on stage, but moves or says something
    Stay StayChange
  deriving (Eq, Generic, Ord, Show)

instance Semigroup ActorChange where
  _ <> change = change

instance Monoid ActorChange where
  mempty = Stay mempty

mkChange :: DirectionChange -> TellingChange -> Int -> Int -> ActorChange
mkChange turn tellingChange xoffset yoffset = Stay StayChange {..}

at :: Int -> Int -> ActorChange
at x y = Stay mempty {xoffset = x, yoffset = y}

at' :: Direction -> Int -> Int -> ActorChange
at' dir x y = Stay mempty {turn = turnFrom dir, xoffset = x, yoffset = y}
  where
    turnFrom ToRight = TurnRight
    turnFrom ToLeft = TurnLeft

down :: ActorChange
down = at 0 1

left :: ActorChange
left = at (-1) 0

right :: ActorChange
right = at 1 0

shutup :: ActorChange
shutup = Stay mempty {tellingChange = ShutUp}

tell :: String -> ActorChange
tell s = Stay mempty {tellingChange = Tell s}

up :: ActorChange
up = at 0 (-1)

initial :: TimedFrame ActorChange -> TimedFrame ActorState
initial tframe = patch (TimedFrame 0 (Frame mempty)) tframe

patch :: TimedFrame ActorState -> TimedFrame ActorChange -> TimedFrame ActorState
patch
  TimedFrame {frame = Frame oldState}
  TimedFrame {duration, frame = Frame diff} =
    -- Take duration from Change: ignore old duration
    TimedFrame {frame = Frame newState, ..}
    where
      newState =
        Map.merge
          Map.preserveMissing
          (Map.mapMaybeMissing (\_ -> patchActorState defaultActorState))
          (Map.zipWithMaybeMatched (\_ -> patchActorState))
          oldState
          diff

patchActorState :: ActorState -> ActorChange -> Maybe ActorState
patchActorState s@ActorState {..} (Stay StayChange {..}) =
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
    applyTellingChange telling NoTellingChange = telling
    applyTellingChange _ (Tell s) = Just s
    applyTellingChange _ ShutUp = Nothing
patchActorState _ Leave = Nothing

(=:) :: Element -> ActorChange -> Frame ActorChange
k =: v = Frame (Map.singleton k v)

instance Semigroup (Frame ActorChange) where
  (Frame m1) <> (Frame m2) = Frame (Map.unionWith (<>) m1 m2)

instance Monoid (Frame ActorChange) where
  mempty = Frame mempty

-- | Given a duration and a frame, builds a 'TimedFrame'
while :: Int -> Frame a -> Scene a
while i m = Scene [TimedFrame {duration = i, frame = m}]

type Date = Int

type DatedFrames =
  ( [(Date, Frame ActorChange)],
    Date -- end date of the last diff
  )

(|||) :: Scene ActorChange -> Scene ActorChange -> Scene ActorChange
(Scene ss1) ||| (Scene ss2) = Scene (fromDates (merge (toDates ss1) (toDates ss2)))
  where
    toDates :: [TimedFrame ActorChange] -> DatedFrames
    toDates = go [] 0
      where
        go acc t [] = (reverse acc, t)
        go acc t (TimedFrame {duration, frame} : scenes) = go ((t, frame) : acc) (t + duration) scenes
    fromDates :: DatedFrames -> [TimedFrame ActorChange]
    fromDates (frames, endDate) = go frames
      where
        go [] = []
        go [(t, frame)] = [TimedFrame (endDate - t) frame]
        go ((t1, frame) : frames@((t2, _) : _)) = TimedFrame (t2 - t1) frame : go frames
    merge :: DatedFrames -> DatedFrames -> DatedFrames
    merge (ms1, endDate1) (ms2, endDate2) = (go ms1 ms2, max endDate1 endDate2)
      where
        go ms1 [] = ms1
        go [] ms2 = ms2
        go ms1@((t1, m1) : ms1') ms2@((t2, m2) : ms2')
          | t1 < t2 = (t1, m1) : go ms1' ms2
          | t1 > t2 = (t2, m2) : go ms1 ms2'
          | otherwise = (t1, m1 <> m2) : go ms1' ms2'

-- | Builds a scene of states from a scene of changes. Interprets the
-- | first diff as an absolute scene (i.e. not as a diff).
display :: Scene ActorChange -> Scene ActorState
display (Scene []) = Scene []
display (Scene (absolute : diffs)) =
  Scene (firstActorState : display' firstActorState diffs)
  where
    firstActorState = initial absolute
    display' _ [] = []
    display' display (firstChange : nextChanges) =
      let nextActorState = patch display firstChange
       in nextActorState : display' nextActorState nextChanges
