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
    TellChange,
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
    patch',
    right,
    shutup,
    tell,
    uncons,
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

data Element
  = -- The actor's unique identifier, and its tile
    Actor Int CreatureID
  | TileElement Tile
  deriving (Eq, Generic, Ord, Show)

newtype Frame a = Frame {unFrame :: Map.Map Element a}
  deriving (Eq, Ord, Show, Generic)

data TimedFrame a = TimedFrame
  { -- | The duration of a scene, in tenth of seconds
    duration :: Int,
    -- | The scene's elements
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

data TellChange = Tell String | ShutUp | NoTellChange
  deriving (Eq, Generic, Ord, Show)

-- last change wins
instance Semigroup TellChange where
  change <> NoTellChange = change
  _ <> change = change

instance Monoid TellChange where
  mempty = NoTellChange

-- | The change to an actor's 'ActorState'
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

mkChange :: DirectionChange -> TellChange -> Int -> Int -> ActorChange
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
initial TimedFrame {duration, frame = Frame changes} =
  TimedFrame {..}
  where
    frame = Map.map initial' changes & Map.mapMaybe id & Frame

initial' :: ActorChange -> Maybe ActorState
initial' (Stay StayChange {..}) =
  Just $
    ActorState
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

patch :: TimedFrame ActorState -> TimedFrame ActorChange -> TimedFrame ActorState
patch
  TimedFrame {frame = Frame prev}
  TimedFrame {duration, frame = Frame diff} =
    -- Take duration from Change: ignore old duration
    TimedFrame {frame = Frame frame'', ..}
    where
      -- Compute elements that make it to the next scene
      elements = Map.keys prev ++ Map.keys diff & Set.fromList
      frame' = Set.map buildActorState elements & Set.toList & Map.fromList
      buildActorState e =
        ( e,
          case (prev Map.!? e, diff Map.!? e) of
            (_, Just Leave) -> Nothing
            (Nothing, Just c) -> initial' c -- No previous state, consider diff as absolute
            (Just p, Just c) -> patch' p c -- Apply diff
            (p, Nothing) -> p -- No diff: keep previous state
        )
      frame'' = Map.mapMaybe id frame'

patch' :: ActorState -> ActorChange -> Maybe ActorState
patch' s@ActorState {..} (Stay StayChange {..}) =
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

-- | Builds a list of displays from a list of diffs. Interprets the
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

uncons :: Scene ActorState -> Maybe (TimedFrame ActorState, Scene ActorState)
uncons (Scene []) = Nothing
uncons (Scene (frame : frames)) = Just (frame, Scene frames)
