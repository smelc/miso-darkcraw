{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
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
    forkWithId,
    joinThread,
    leave,
    left,
    mkChange,
    newActor,
    patch,
    render,
    right,
    shutup,
    spriteToKind,
    tell,
    tileSprite,
    turnAround,
    up,
    while,
    during,
    getActorState,
    dot,
  )
where

import Card (CreatureID)
import Control.Lens ((%=), use)
import Control.Monad.Operational
import qualified Control.Monad.State.Strict as MTL
import qualified Control.Monad.Writer.Strict as MTL
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import Data.Semigroup (Any (..))
import qualified Data.Set as Set
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

newtype ThreadId = ThreadId Int
  deriving (Eq, Ord, Show, Generic)

data SceneInstruction :: * -> * where
  NewActor :: SceneInstruction Element
  While :: Duration -> Frame ActorChange -> SceneInstruction ()
  Fork :: Scene () -> SceneInstruction ThreadId
  Join :: ThreadId -> SceneInstruction ()
  GetActorState :: Element -> SceneInstruction ActorState

type Scene = Program SceneInstruction

newActor :: Scene Element
newActor = singleton NewActor

while :: Duration -> Frame ActorChange -> Scene ()
while duration frame = singleton (While duration frame)

during :: Duration -> FrameDiff () -> Scene ()
during duration (FrameDiff m) = singleton (While duration (MTL.execWriter m))

(|||) :: Scene () -> Scene () -> Scene ()
s1 ||| s2 = do
  tid <- forkWithId s1
  s2
  joinThread tid

forkWithId :: Scene () -> Scene ThreadId
forkWithId scene = singleton (Fork scene)

fork :: Scene () -> Scene ()
fork scene = do
  _ <- forkWithId scene
  return ()

joinThread :: ThreadId -> Scene ()
joinThread threadId = singleton (Join threadId)

getActorState :: Element -> Scene ActorState
getActorState element = singleton (GetActorState element)

-- | Typical usage: archerX <- archer `dot` x
dot :: Element -> (ActorState -> a) -> Scene a
dot element proj = proj <$> getActorState element

type Date = Int

type Prog = ProgramView SceneInstruction ()

data Condition = WaitingForDate Date | WaitingForThreadId ThreadId

data Thread = Thread {threadId :: ThreadId, threadCondition :: Condition, threadProg :: Prog}

data SchedulerState = SchedulerState {actorCounter :: Int, threadCounter :: Int}
  deriving (Eq, Ord, Show, Generic)

type Scheduler = MTL.State SchedulerState

type Stepper = MTL.WriterT (Maybe (Frame ActorChange)) Scheduler

type DatedFrames =
  ( [(Date, Frame ActorState)],
    Date -- end date of the last diff
  )

render :: Scene () -> [TimedFrame ActorState]
render scene =
  datesToDurations
    $ flip MTL.evalState (SchedulerState 0 1)
    $ eval 0 (Frame mempty) [Thread (ThreadId 0) (WaitingForDate 0) (view scene)]
  where
    eval :: Date -> Frame ActorState -> [Thread] -> Scheduler DatedFrames
    eval lastDate _ [] = return ([], lastDate)
    eval lastDate frame threads
      | null dates = return ([], lastDate)
      | otherwise = do
        let date = minimum dates
        (newThreads, mdiff) <- MTL.runWriterT (advanceThreads frame date threads)
        case mdiff of
          Just diff -> do
            let newFrame = patch frame diff
            (timedFrames, newLastDate) <- eval date newFrame newThreads
            return ((date, newFrame) : timedFrames, newLastDate)
          Nothing ->
            eval date frame newThreads
      where
        dates = [date | Thread _ (WaitingForDate date) _ <- threads]
    advanceThreads :: Frame ActorState -> Date -> [Thread] -> Stepper [Thread]
    advanceThreads frame now threads = stepThreads frame now threads >>= loop
      where
        loop (Any False, unchangedThreads) = return unchangedThreads
        loop (Any True, newThreads) = stepThreads frame now newThreads >>= loop
    stepThreads :: Frame ActorState -> Date -> [Thread] -> Stepper (Any, [Thread])
    stepThreads frame now threads = mconcat <$> mapM step threads
      where
        step thread@(Thread tid condition prog)
          | WaitingForDate date <- condition, now == date = (Any True,) <$> stepThread frame now tid prog
          | WaitingForThreadId i <- condition, not (Set.member i threadIds) = (Any True,) <$> stepThread frame now tid prog
          | otherwise = return (Any False, [thread])
        threadIds = Set.fromList (map threadId threads)
    stepThread :: Frame ActorState -> Date -> ThreadId -> Prog -> Stepper [Thread]
    stepThread frame@(Frame actors) now tid prog =
      case prog of
        Return () ->
          return []
        NewActor :>>= k -> do
          i <- use #actorCounter
          #actorCounter %= (+ 1)
          stepThread frame now tid (view (k (Element i)))
        While duration diff :>>= k -> do
          MTL.tell (Just diff)
          return [Thread tid (WaitingForDate (now + duration)) (view (k ()))]
        Fork scene :>>= k -> do
          i <- ThreadId <$> use #threadCounter
          #threadCounter %= (+ 1)
          concat <$> sequence [stepThread frame now i (view scene), stepThread frame now tid (view (k i))]
        Join i :>>= k ->
          return [Thread tid (WaitingForThreadId i) (view (k ()))]
        GetActorState aid :>>= k ->
          stepThread frame now tid (view (k (actors Map.! aid)))
    datesToDurations :: DatedFrames -> [TimedFrame ActorState]
    datesToDurations (frames, endDate) = go frames
      where
        go [] = []
        go [(t, frame)] = [TimedFrame (endDate - t) frame]
        go ((t1, frame) : frames@((t2, _) : _)) = TimedFrame (t2 - t1) frame : go frames

data DirectionChange = NoDirectionChange | ToggleDir | TurnRight | TurnLeft
  deriving (Eq, Generic, Ord, Show)

instance Semigroup DirectionChange where
  change <> NoDirectionChange = change
  TurnLeft <> ToggleDir = TurnRight
  TurnRight <> ToggleDir = TurnLeft
  ToggleDir <> ToggleDir = NoDirectionChange
  _ <> change = change

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
