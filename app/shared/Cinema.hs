{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Module containing the base for animating scenes in lobbies
module Cinema
  ( ActorKind (..),
    ActorChange (),
    ActorState (..),
    Direction (..),
    DirectionChange (),
    Element (),
    Frame (..),
    FrameDiff (),
    Scene (..),
    SpriteChange (),
    TellingChange (),
    TimedFrame (..),
    (+=),
    (|||),
    at,
    at',
    creatureSprite,
    defaultDirection,
    dress,
    down,
    fork,
    leave,
    left,
    mkChange,
    newActor,
    render,
    right,
    runScene,
    shutup,
    spriteToKind,
    tell,
    tileSprite,
    turnAround,
    up,
    while,
    during,
  )
where

import Card (CreatureID)
import Control.Monad.Operational
import qualified Control.Monad.State.Strict as MTL
import qualified Control.Monad.Writer.Strict as MTL
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Tile (Tile)

data Direction = ToLeft | ToRight -- Suffix with 'To' to avoid clashing with Either
  deriving (Eq, Generic, Ord, Show)

defaultDirection :: Direction
defaultDirection = ToLeft -- How sprite are in oryx' set

creatureSprite :: CreatureID -> Sprite
creatureSprite = Left

tileSprite :: Tile -> Sprite
tileSprite = Right

data ActorKind = CreatureKind | TileKind
  deriving (Eq, Ord, Show)

-- Sufficient data to obtain a sprite
type Sprite = Either CreatureID Tile

spriteToKind :: Sprite -> ActorKind
spriteToKind (Left _) = CreatureKind
spriteToKind _ = TileKind

data ActorState = ActorState
  { -- | In which direction the sprite is looking at
    direction :: Direction,
    -- | How to draw this actor
    sprite :: Maybe Sprite,
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
      sprite = Nothing,
      telling = Nothing,
      x = 0,
      y = 0
    }

-- An actor's unique identifier
newtype Element = Element Int
  deriving (Eq, Generic, Ord, Show)

newtype FrameDiff a = FrameDiff {unFrameDiff :: MTL.Writer (Frame ActorChange) a}
  deriving (Eq, Show, Functor, Applicative, Monad)

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

data SceneInstruction :: * -> * where
  NewActor :: SceneInstruction Element
  While :: Duration -> Frame ActorChange -> SceneInstruction ()
  Parallelize :: Scene () -> Scene () -> SceneInstruction ()
  Fork :: Scene () -> SceneInstruction ()

type Scene = Program SceneInstruction

newActor :: Scene Element
newActor = singleton NewActor

while :: Duration -> Frame ActorChange -> Scene ()
while duration frame = singleton (While duration frame)

during :: Duration -> FrameDiff () -> Scene ()
during duration (FrameDiff m) = singleton (While duration (MTL.execWriter m))

(|||) :: Scene () -> Scene () -> Scene ()
s1 ||| s2 = singleton (Parallelize s1 s2)

fork :: Scene () -> Scene ()
fork scene = singleton (Fork scene)

type LowLevelScene a = MTL.WriterT [TimedFrame ActorChange] (MTL.State Int) a

lower :: Scene () -> LowLevelScene ()
lower scene = go scene
  where
    go :: Scene () -> LowLevelScene ()
    go scene = eval (view scene)
    eval :: ProgramView SceneInstruction () -> LowLevelScene ()
    eval (Return a) = return a
    eval (NewActor :>>= k) = do
      i <- MTL.get
      MTL.modify (const (i + 1))
      go (k (Element i))
    eval (While duration frame :>>= k) = do
      MTL.tell [TimedFrame duration frame]
      go (k ())
    eval (Parallelize s1 s2 :>>= k) = do
      parallelizeLowLevelScenes (lower s1) (lower s2)
      go (k ())
    eval (Fork scene :>>= k) =
      parallelizeLowLevelScenes (lower scene) (go (k ()))
    parallelizeLowLevelScenes :: LowLevelScene () -> LowLevelScene () -> LowLevelScene ()
    parallelizeLowLevelScenes s1 s2 = do
      frames1 <- MTL.lift (MTL.execWriterT s1)
      frames2 <- MTL.lift (MTL.execWriterT s2)
      MTL.tell (parallelize frames1 frames2)

runScene :: Scene () -> [TimedFrame ActorChange]
runScene scene = MTL.evalState (MTL.execWriterT (lower scene)) 0

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

data SpriteChange = SetSprite Sprite | HideSprite | NoSpriteChange
  deriving (Eq, Generic, Ord, Show)

-- | The change to an actor's 'ActorState'
data ActorChange = ActorChange
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

instance Semigroup ActorChange where
  (ActorChange turn1 sprite1 tell1 dx1 dy1) <> (ActorChange turn2 sprite2 tell2 dx2 dy2) =
    ActorChange (turn1 <> turn2) (sprite1 <> sprite2) (tell1 <> tell2) (dx1 + dx2) (dy1 + dy2)

instance Monoid ActorChange where
  mempty = ActorChange mempty mempty mempty 0 0

mkChange :: Sprite -> DirectionChange -> TellingChange -> Int -> Int -> ActorChange
mkChange sprite turn tellingChange xoffset yoffset =
  ActorChange {spriteChange = SetSprite sprite, ..}

(+=) :: Element -> ActorChange -> FrameDiff ()
actor += change = FrameDiff (MTL.tell (Frame (Map.singleton actor change)))

at :: Sprite -> Int -> Int -> ActorChange
at sprite x y = mempty {spriteChange = SetSprite sprite, xoffset = x, yoffset = y}

at' :: Sprite -> Direction -> Int -> Int -> ActorChange
at' sprite dir x y = mempty {spriteChange = SetSprite sprite, turn = turnFrom dir, xoffset = x, yoffset = y}
  where
    turnFrom ToRight = TurnRight
    turnFrom ToLeft = TurnLeft

dress :: Element -> Sprite -> FrameDiff ()
dress actor sprite = actor += mempty {spriteChange = SetSprite sprite}

shift :: Element -> Int -> Int -> FrameDiff ()
shift actor x y = actor += mempty {xoffset = x, yoffset = y}

down :: Element -> FrameDiff ()
down actor = shift actor 0 1

left :: Element -> FrameDiff ()
left actor = shift actor (-1) 0

right :: Element -> FrameDiff ()
right actor = shift actor 1 0

shutup :: Element -> FrameDiff ()
shutup actor = actor += mempty {tellingChange = ShutUp}

tell :: Element -> String -> FrameDiff ()
tell actor s = actor += mempty {tellingChange = Tell s}

turnAround :: Element -> FrameDiff ()
turnAround actor = actor += mempty {turn = ToggleDir}

up :: Element -> FrameDiff ()
up actor = shift actor 0 (-1)

leave :: Element -> FrameDiff ()
leave actor = actor += mempty {spriteChange = HideSprite}

patch :: Frame ActorState -> Frame ActorChange -> Frame ActorState
patch (Frame oldState) (Frame diff) = Frame newState
  where
    newState =
      Map.merge
        Map.preserveMissing
        (Map.mapMissing (const (patchActorState defaultActorState)))
        (Map.zipWithMatched (const patchActorState))
        oldState
        diff

patchActorState :: ActorState -> ActorChange -> ActorState
patchActorState s@ActorState {..} ActorChange {..} =
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
    applySpriteChange _ (SetSprite s) = Just s
    applySpriteChange _ HideSprite = Nothing
    applySpriteChange s NoSpriteChange = s
    applyTellingChange telling NoTellingChange = telling
    applyTellingChange _ (Tell s) = Just s
    applyTellingChange _ ShutUp = Nothing
    otherDir ToRight = ToLeft
    otherDir ToLeft = ToRight

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
