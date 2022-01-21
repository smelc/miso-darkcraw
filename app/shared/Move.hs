{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |
-- This module deals with game actions (nicknamed moves). It's a spinoff of
-- 'Update', to reduce the length of the latter.
module Move where

import qualified Board
import Control.Monad.Except (MonadError, throwError)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified Game
import Miso.String (MisoString)
import Nat

data DnDAction a
  = -- | Dragging card in hand ends. When a successful drop is done,
    -- this event is fired right after 'Drop'. We rely on that. If Drop
    -- was fired last, we would miss it. Be careful on untested browsers.
    DragEnd
  | -- | Dragging card in hand
    DragStart Board.HandIndex
  | -- | In 'GameView', this can play the 'GamePlayEvent' 'Place'
    Drop
  | DragEnter a
  | DragLeave a
  deriving (Show, Eq)

-- | Actions that are raised by 'GameView'
data Move
  = -- | A drag an drop event
    DnD (DnDAction Game.Target)
  | -- | Play some game event. It can be an event scheduled by the AI
    -- or an event from the player.
    Play Game.Event
  | -- | Turn was updated previously by 'IncrTurn',
    -- time to draw cards from the stack. Then the handler of this event
    -- will take care of giving the player control back. This event
    -- is translated to a list of events, iteratively consuming the list.
    DrawCards Game.DrawSource
  | -- | All actions have been resolved, time to update the turn widget
    -- and to schedule 'DrawCard'. This does NOT translate
    -- to a 'PlayEvent'.
    IncrTurn
  | -- | End Turn button pressed in turn widget. For player, schedule
    -- attacks then 'IncrTurn'; for AI, compute its actions,
    -- schedule them, and then schedule attack and 'IncrTurn'.
    EndTurnPressed
  | -- | Starting hovering card in hand
    InHandMouseEnter Board.HandIndex
  | -- | Ending hovering card in hand
    InHandMouseLeave Board.HandIndex
  | -- | Starting hovering a target
    InPlaceMouseEnter Game.Target
  | -- | Ending hovering a target
    InPlaceMouseLeave Game.Target
  | -- | Execute a command (dev mode only)
    ExecuteCmd
  | -- A number of events to apply in sequence.
    Sequence (NonEmpty Move)
  | -- | Update the command to execute soon (dev mode only)
    UpdateCmd MisoString
  deriving (Show, Eq)

-- | Given a constructor of 'Move' that expects a non empty list and
-- a list; the corresponding event; if any. If the input list is empty,
-- 'Nothing' is returned.
eventsToSeq :: (NonEmpty a -> Move) -> [a] -> NextMove
eventsToSeq f as = fmap (1,) $ eventsToAction f as

-- | Given a constructor of 'Move' that expects a non empty list and
-- a list; the corresponding action; if any. If the input list is empty,
-- 'Nothing' is returned.
eventsToAction :: (NonEmpty a -> Move) -> [a] -> Maybe Move
eventsToAction f as = fmap f $ NE.nonEmpty as

actionsToSequence :: [Move] -> Maybe Move
actionsToSequence l =
  case NE.nonEmpty l of
    Nothing -> Nothing
    Just l -> Just (Sequence l)

-- | Event to fire after the given delay (in seconds). Delay should not be '0'.
type NextMove = Maybe (Nat, Move)

-- | Given (maybe) an action and other actions, build the sequence
consNgaActions :: NextMove -> [Move] -> NextMove
consNgaActions nga actions =
  case (nga, NE.nonEmpty actions) of
    (Nothing, Nothing) -> Nothing
    (Nothing, Just neActions) -> Just (1, Sequence neActions)
    (Just pair, Nothing) -> Just pair
    (Just (n, ga), Just neActions) -> Just (n, Sequence (ga NE.<| neActions))

mkNextMove ::
  MonadError Text.Text m =>
  Nat ->
  Move ->
  m NextMove
mkNextMove delay a
  | delay == 0 = throwError "Delay should not be 0"
  | otherwise = pure $ Just (delay, a)
