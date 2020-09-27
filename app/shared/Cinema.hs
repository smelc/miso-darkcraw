{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Module containing the base for animating scenes in lobbies
module Cinema
  ( ActorChange (Leave),
    Direction (..),
    DirectionChange,
    Element (TileElement, Actor_),
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
    newActor,
    defaultDirection,
    render,
    down,
    initial,
    left,
    mkChange,
    patch,
    patchActorState,
    right,
    runScene,
    shutup,
    tell,
    up,
    while,
  )
where

import Card (CreatureID)
import Control.Monad.Operational
import Control.Monad.State
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

{-# COMPLETE TileElement, Actor_ #-}

data Element
  = -- The actor's unique identifier, and its tile
    Actor Int CreatureID
  | TileElement Tile
  deriving (Eq, Generic, Ord, Show)

-- We define a pattern synonym for Actor so that we can export
-- Actor as "read-only": creating a new Actor is only allowed
-- via newActor.
pattern Actor_ :: Int -> CreatureID -> Element
pattern Actor_ id cid <- Actor id cid

newtype Frame a = Frame {unFrame :: Map.Map Element a}
  deriving (Eq, Ord, Show, Generic)

data TimedFrame a = TimedFrame
  { -- | The duration of a frame, in tenth of seconds
    duration :: Duration,
    -- | The frame
    frame :: Frame a
  }
  deriving (Eq, Ord, Show, Generic)

type Duration = Int

data SceneInstruction a where
  CreateActor :: CreatureID -> SceneInstruction Element
  AddTimedFrame :: Duration -> Frame ActorChange -> SceneInstruction ()
  RunInParallel :: Scene () -> Scene () -> SceneInstruction ()

type Scene a = Program SceneInstruction a

newActor :: CreatureID -> Scene Element
newActor = singleton . CreateActor

-- | Given a duration and a frame, builds a 'TimedFrame'
while :: Duration -> Frame ActorChange -> Scene ()
while i m = singleton (AddTimedFrame i m)

(|||) :: Scene () -> Scene () -> Scene ()
s1 ||| s2 = singleton (RunInParallel s1 s2)

runScene :: Scene () -> [TimedFrame ActorChange]
runScene m = evalState (go m) (0 :: Int)
  where
    go :: Scene () -> State Int [TimedFrame ActorChange]
    go scene = eval (view scene)
    eval :: ProgramView SceneInstruction () -> State Int [TimedFrame ActorChange]
    eval (Return ()) = return []
    eval (CreateActor cid :>>= k) = do
      i <- get
      let actor = Actor i cid
      modify (const (i + 1))
      go (k actor)
    eval (AddTimedFrame duration diff :>>= k) = do
      timedFrames <- go (k ())
      return (TimedFrame duration diff : timedFrames)
    eval (RunInParallel scene1 scene2 :>>= k) = do
      frames1 <- go scene1
      frames2 <- go scene2
      tail <- go (k ())
      return $ (parallelize frames1 frames2) ++ tail

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
initial (TimedFrame duration frame) = TimedFrame duration (patch (Frame mempty) frame)

patch :: Frame ActorState -> Frame ActorChange -> Frame ActorState
patch (Frame oldState) (Frame diff) = Frame newState
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

type Date = Int

type DatedFrames a =
  ( [(Date, Frame a)],
    Date -- end date of the last diff
  )

parallelize :: Semigroup (Frame a) => [TimedFrame a] -> [TimedFrame a] -> [TimedFrame a]
parallelize ss1 ss2 = fromDates (merge (toDates ss1) (toDates ss2))
  where
    toDates :: [TimedFrame a] -> DatedFrames a
    toDates = go [] 0
      where
        go acc t [] = (reverse acc, t)
        go acc t (TimedFrame {duration, frame} : scenes) = go ((t, frame) : acc) (t + duration) scenes
    fromDates :: DatedFrames a -> [TimedFrame a]
    fromDates (frames, endDate) = go frames
      where
        go [] = []
        go [(t, frame)] = [TimedFrame (endDate - t) frame]
        go ((t1, frame) : frames@((t2, _) : _)) = TimedFrame (t2 - t1) frame : go frames
    merge :: Semigroup (Frame a) => DatedFrames a -> DatedFrames a -> DatedFrames a
    merge (ms1, endDate1) (ms2, endDate2) = (go ms1 ms2, max endDate1 endDate2)
      where
        go ms1 [] = ms1
        go [] ms2 = ms2
        go ms1@((t1, m1) : ms1') ms2@((t2, m2) : ms2')
          | t1 < t2 = (t1, m1) : go ms1' ms2
          | t1 > t2 = (t2, m2) : go ms1 ms2'
          | otherwise = (t1, m1 <> m2) : go ms1' ms2'

render :: Scene () -> [TimedFrame ActorState]
render scene = go (Frame mempty) (runScene scene)
  where
    go _ [] = []
    go frame (TimedFrame duration diff : tdiffs) = TimedFrame duration newFrame : go newFrame tdiffs
      where
        newFrame = patch frame diff
