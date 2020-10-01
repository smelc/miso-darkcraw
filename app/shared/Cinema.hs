{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Module containing the base for animating scenes in lobbies
module Cinema
  ( ActorChange (Leave),
    Direction (..),
    DirectionChange,
    Element (),
    Frame (..),
    ActorState (..),
    Scene (..),
    SpriteChange,
    StayChange,
    TellingChange,
    TimedFrame (..),
    (=:),
    (|||),
    at,
    at',
    creatureSprite,
    defaultDirection,
    dress,
    down,
    left,
    mkChange,
    newActor,
    render,
    right,
    runScene,
    shutup,
    tell,
    tileSprite,
    turnAround,
    up,
    while,
  )
where

import Card (CreatureID)
import qualified Control.Monad.State.Strict as MTL
import qualified Control.Monad.Writer.Strict as MTL
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

creatureSprite :: CreatureID -> Sprite
creatureSprite = Left

tileSprite :: Tile -> Sprite
tileSprite = Right

-- Sufficient data to obtain a sprite
type Sprite = Either CreatureID Tile

data ActorState = ActorState
  { -- | In which direction the sprite is looking at
    direction :: Direction,
    -- | How to draw this actor
    sprite :: Sprite,
    -- | Whether the element says something
    telling :: Maybe String,
    -- | 0 means on the left
    x :: Int,
    -- | 0 means top
    y :: Int
  }
  deriving (Eq, Generic, Ord, Show)

defaultActorState :: Sprite -> ActorState
defaultActorState sprite =
  ActorState
    { direction = defaultDirection,
      sprite = sprite,
      telling = Nothing,
      x = 0,
      y = 0
    }

-- An actor's unique identifier
newtype Element = Element Int
  deriving (Eq, Generic, Ord, Show)

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

newtype Scene a = Scene (MTL.WriterT [TimedFrame ActorChange] (MTL.State Int) a)
  deriving (Functor, Applicative, Monad)

newActor :: Scene Element
newActor = Scene $ do
  i <- MTL.get
  MTL.modify (const (i + 1))
  return $ Element i

-- | Given a duration and a frame, builds a 'TimedFrame'
while :: Duration -> Frame ActorChange -> Scene ()
while duration frame =
  Scene $
    MTL.tell [TimedFrame duration frame]

(|||) :: Scene () -> Scene () -> Scene ()
(Scene s1) ||| (Scene s2) = Scene $ do
  frames1 <- MTL.lift (MTL.execWriterT s1)
  frames2 <- MTL.lift (MTL.execWriterT s2)
  MTL.tell (parallelize frames1 frames2)

runScene :: Scene () -> [TimedFrame ActorChange]
runScene (Scene m) = MTL.evalState (MTL.execWriterT m) 0

data DirectionChange = NoDirectionChange | ToggleDir | TurnRight | TurnLeft
  deriving (Eq, Generic, Ord, Show)

instance Semigroup DirectionChange where
  TurnLeft <> TurnRight = NoDirectionChange
  TurnLeft <> ToggleDir = NoDirectionChange
  TurnLeft <> _ = TurnLeft
  TurnRight <> TurnLeft = NoDirectionChange
  TurnRight <> ToggleDir = NoDirectionChange
  TurnRight <> _ = TurnRight
  ToggleDir <> TurnLeft = NoDirectionChange
  ToggleDir <> TurnRight = NoDirectionChange
  ToggleDir <> _ = ToggleDir
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

data SpriteChange = SetSprite Sprite | NoSpriteChange
  deriving (Eq, Generic, Ord, Show)

-- | The change to an actor's 'ActorState'
data StayChange = StayChange
  { -- | The change to 'direction'
    turn :: DirectionChange,
    -- | The change to the sprite
    spriteChange :: SpriteChange,
    -- | The change to 'telling'
    tellingChange :: TellingChange,
    -- | The change to 'x'
    xoffset :: Int,
    -- | The change to 'y'
    yoffset :: Int
  }
  deriving (Eq, Generic, Ord, Show)

instance Semigroup SpriteChange where
  s <> NoSpriteChange = s
  _ <> s = s

instance Monoid SpriteChange where
  mempty = NoSpriteChange

instance Semigroup StayChange where
  (StayChange turn1 sprite1 tell1 dx1 dy1) <> (StayChange turn2 sprite2 tell2 dx2 dy2) =
    StayChange (turn1 <> turn2) (sprite1 <> sprite2) (tell1 <> tell2) (dx1 + dx2) (dy1 + dy2)

instance Monoid StayChange where
  mempty = StayChange mempty mempty mempty 0 0

data ActorChange
  = -- | Actor leaves the stage
    Leave
  | -- | Actor stays on stage, but moves or says something
    Stay StayChange
  deriving (Eq, Generic, Ord, Show)

instance Semigroup ActorChange where
  Leave <> _ = Leave
  _ <> Leave = Leave
  Stay change1 <> Stay change2 = Stay (change1 <> change2)

instance Monoid ActorChange where
  mempty = Stay mempty

mkChange :: Sprite -> DirectionChange -> TellingChange -> Int -> Int -> ActorChange
mkChange sprite turn tellingChange xoffset yoffset =
  Stay StayChange {spriteChange = SetSprite sprite, ..}

-- | Use this function to initialize an 'Element'
at :: Sprite -> Int -> Int -> ActorChange
at sprite x y = Stay mempty {spriteChange = SetSprite sprite, xoffset = x, yoffset = y}

-- | Use this function to initialize an 'Element'
at' :: Sprite -> Direction -> Int -> Int -> ActorChange
at' sprite dir x y =
  Stay mempty {spriteChange = SetSprite sprite, turn = turnFrom dir, xoffset = x, yoffset = y}
  where
    turnFrom ToRight = TurnRight
    turnFrom ToLeft = TurnLeft

dress :: Sprite -> ActorChange
dress sprite = Stay mempty {spriteChange = SetSprite sprite}

shift :: Int -> Int -> ActorChange
shift x y = Stay mempty {xoffset = x, yoffset = y}

down :: ActorChange
down = shift 0 1

left :: ActorChange
left = shift (-1) 0

right :: ActorChange
right = shift 1 0

shutup :: ActorChange
shutup = Stay mempty {tellingChange = ShutUp}

tell :: String -> ActorChange
tell s = Stay mempty {tellingChange = Tell s}

turnAround :: ActorChange
turnAround = Stay mempty {turn = ToggleDir}

up :: ActorChange
up = shift 0 (-1)

patch :: Frame ActorState -> Frame ActorChange -> Frame ActorState
patch (Frame oldState) (Frame diff) = Frame newState
  where
    newState =
      Map.merge
        Map.preserveMissing
        (Map.mapMaybeMissing mapRight)
        (Map.zipWithMaybeMatched (const patchActorState))
        oldState
        diff
    mapRight :: Element -> ActorChange -> Maybe ActorState
    mapRight k Leave = Nothing
    mapRight k c@(Stay StayChange {..}) =
      case spriteChange of
        NoSpriteChange -> error $ show k ++ " has no sprite"
        SetSprite sprite -> patchActorState (defaultActorState sprite) c

patchActorState :: ActorState -> ActorChange -> Maybe ActorState
patchActorState s@ActorState {..} (Stay StayChange {..}) =
  Just $
    s
      { direction = applyDirectionChange direction turn,
        sprite = applySpriteChange sprite spriteChange,
        telling = applyTellingChange telling tellingChange,
        x = x + xoffset,
        y = y + yoffset
      }
  where
    applyDirectionChange dir NoDirectionChange = dir
    applyDirectionChange dir ToggleDir = otherDir dir
    applyDirectionChange _ TurnRight = ToRight
    applyDirectionChange _ TurnLeft = ToLeft
    applySpriteChange _ (SetSprite s) = s
    applySpriteChange s NoSpriteChange = s
    applyTellingChange telling NoTellingChange = telling
    applyTellingChange _ (Tell s) = Just s
    applyTellingChange _ ShutUp = Nothing
    otherDir ToRight = ToLeft
    otherDir ToLeft = ToRight
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
