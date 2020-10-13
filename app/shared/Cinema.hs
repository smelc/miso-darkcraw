{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Module containing the base for animating scenes in lobbies
module Cinema
  ( ActorKind (..),
    ActorState (..),
    Direction (..),
    Element (),
    Frame (..),
    Scene (),
    TimedFrame (..),
    (|||),
    creatureSprite,
    defaultDirection,
    dress,
    down,
    dot,
    getActorState,
    fork,
    forkWithId,
    joinThread,
    hide,
    left,
    newActor,
    newActorAt,
    newActorAt',
    render,
    right,
    setActorState,
    shutup,
    spriteToKind,
    tell,
    tileSprite,
    turnAround,
    up,
    during,
    (=:),
    newHiddenActor,
    moveTo,
    resetAt,
    turnTo,
    resetAt',
    wait,
  )
where

import Card (CreatureID)
import Control.Lens ((%=), (%~), (&), (+~), (.~), (?=), (?~), use)
import qualified Control.Lens as Lens
import Control.Monad.Operational
import qualified Control.Monad.State.Strict as MTL
import qualified Control.Monad.Writer.Strict as MTL
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

newtype ThreadId = ThreadId Int
  deriving (Eq, Ord, Show, Generic)

data SceneInstruction :: * -> * where
  NewActor :: ActorState -> SceneInstruction Element
  Fork :: Scene () -> SceneInstruction ThreadId
  Join :: ThreadId -> SceneInstruction ()
  GetActorState :: Element -> SceneInstruction ActorState
  Wait :: Duration -> SceneInstruction ()
  SetActorState :: Element -> ActorState -> SceneInstruction ()

type Scene = Program SceneInstruction

newActor :: ActorState -> Scene Element
newActor state = singleton (NewActor state)

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

setActorState :: Element -> ActorState -> Scene ()
setActorState element state = singleton (SetActorState element state)

wait :: Duration -> Scene ()
wait duration = singleton (Wait duration)

type Date = Int

type Prog = ProgramView SceneInstruction ()

data Condition = WaitingForDate Date | WaitingForThreadId ThreadId

data Thread = Thread {threadId :: ThreadId, threadCondition :: Condition, threadProg :: Prog}

data SchedulerState = SchedulerState {actorCounter :: Int, threadCounter :: Int}
  deriving (Eq, Ord, Show, Generic)

type Scheduler = MTL.State SchedulerState

type Stepper = MTL.WriterT Any (MTL.StateT (Frame ActorState) Scheduler)

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
        ((newThreads, Any patched), newFrame) <- MTL.runStateT (MTL.runWriterT (advanceThreads date threads)) frame
        if patched
          then do
            (timedFrames, newLastDate) <- eval date newFrame newThreads
            return ((date, newFrame) : timedFrames, newLastDate)
          else eval date frame newThreads
      where
        dates = [date | Thread _ (WaitingForDate date) _ <- threads]
    advanceThreads :: Date -> [Thread] -> Stepper [Thread]
    advanceThreads now threads = stepThreads now threads >>= loop
      where
        loop (Any False, unchangedThreads) = return unchangedThreads
        loop (Any True, newThreads) = stepThreads now newThreads >>= loop
    stepThreads :: Date -> [Thread] -> Stepper (Any, [Thread])
    stepThreads now threads = mconcat <$> mapM step threads
      where
        step thread@(Thread tid condition prog)
          | WaitingForDate date <- condition, now == date = (Any True,) <$> stepThread now tid prog
          | WaitingForThreadId i <- condition, not (Set.member i threadIds) = (Any True,) <$> stepThread now tid prog
          | otherwise = return (Any False, [thread])
        threadIds = Set.fromList (map threadId threads)
    stepThread :: Date -> ThreadId -> Prog -> Stepper [Thread]
    stepThread now tid prog =
      case prog of
        Return () ->
          return []
        NewActor state :>>= k -> do
          aid <- Element <$> (MTL.lift $ MTL.lift $ use #actorCounter)
          MTL.lift $ MTL.lift $ #actorCounter %= (+ 1)
          #unFrame . Lens.at aid ?= state
          MTL.tell (Any True)
          stepThread now tid (view (k aid))
        SetActorState element state :>>= k -> do
          #unFrame . Lens.at element ?= state
          MTL.tell (Any True)
          concat <$> sequence [stepThread now tid (view (k ()))]
        Fork scene :>>= k -> do
          i <- MTL.lift $ MTL.lift $ ThreadId <$> use #threadCounter
          MTL.lift $ MTL.lift $ #threadCounter %= (+ 1)
          concat <$> sequence [stepThread now i (view scene), stepThread now tid (view (k i))]
        Join i :>>= k ->
          return [Thread tid (WaitingForThreadId i) (view (k ()))]
        GetActorState aid :>>= k -> do
          state <- MTL.gets ((Map.! aid) . unFrame)
          stepThread now tid (view (k state))
        Wait duration :>>= k ->
          return [Thread tid (WaitingForDate (now + duration)) (view (k ()))]
    datesToDurations :: DatedFrames -> [TimedFrame ActorState]
    datesToDurations (frames, endDate) = go frames
      where
        go [] = []
        go [(t, frame)] = [TimedFrame (endDate - t) frame]
        go ((t1, frame) : frames@((t2, _) : _)) = TimedFrame (t2 - t1) frame : go frames

during :: Duration -> Scene a -> Scene a
during duration scene = do
  x <- scene
  wait duration
  return x

(|||) :: Scene () -> Scene () -> Scene ()
s1 ||| s2 = do
  tid <- forkWithId s1
  s2
  joinThread tid

(=:) :: Element -> ActorState -> Scene ()
(=:) = setActorState

-- | Typical usage: archerX <- archer `dot` x
dot :: Element -> (ActorState -> a) -> Scene a
dot element proj = proj <$> getActorState element

modifyActorState :: Element -> (ActorState -> ActorState) -> Scene ()
modifyActorState element f = do
  state <- getActorState element
  setActorState element (f state)

newHiddenActor :: Scene Element
newHiddenActor = newActor $ ActorState {direction = defaultDirection, sprite = Nothing, telling = Nothing, x = 0, y = 0}

newActorAt :: Sprite -> Int -> Int -> Scene Element
newActorAt sprite x y = do
  actor <- newHiddenActor
  actor & resetAt sprite x y
  return actor

newActorAt' :: Sprite -> Direction -> Int -> Int -> Scene Element
newActorAt' sprite direction x y = do
  actor <- newHiddenActor
  actor & resetAt' sprite direction x y
  return actor

resetAt :: Sprite -> Int -> Int -> Element -> Scene ()
resetAt sprite x y actor = do
  actor & dress sprite
  actor & moveTo x y

resetAt' :: Sprite -> Direction -> Int -> Int -> Element -> Scene ()
resetAt' sprite direction x y actor = do
  actor & turnTo direction
  actor & resetAt sprite x y

dress :: Sprite -> Element -> Scene ()
dress sprite actor = modifyActorState actor (#sprite ?~ sprite)

moveTo :: Int -> Int -> Element -> Scene ()
moveTo x y actor = modifyActorState actor ((#x +~ x) . (#y +~ y))

shift :: Int -> Int -> Element -> Scene ()
shift dx dy actor = modifyActorState actor ((#x +~ dx) . (#y +~ dy))

up :: Element -> Scene ()
up = shift 0 (-1)

down :: Element -> Scene ()
down = shift 0 1

left :: Element -> Scene ()
left = shift (-1) 0

right :: Element -> Scene ()
right = shift 1 0

shutup :: Element -> Scene ()
shutup actor = modifyActorState actor (#telling .~ Nothing)

tell :: String -> Element -> Scene ()
tell s actor = modifyActorState actor (#telling ?~ s)

turnTo :: Direction -> Element -> Scene ()
turnTo direction actor = modifyActorState actor (#direction .~ direction)

turnAround :: Element -> Scene ()
turnAround actor = modifyActorState actor (#direction %~ flipDirection)
  where
    flipDirection ToLeft = ToRight
    flipDirection ToRight = ToLeft

hide :: Element -> Scene ()
hide actor = modifyActorState actor (#sprite .~ Nothing)
