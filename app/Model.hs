{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model where

import Board
import Card
import qualified Data.Text as Text
import GHC.Generics

newtype HandIndex = HandIndex {unHandIndex :: Int}
  deriving (Eq, Show, Generic, Enum)

data Interaction
  = -- | Hovering over a card
    HoverInteraction Hovering
  | -- | Dragging a card
    DragInteraction Dragging
  | NoInteraction
  | ShowErrorInteraction Text.Text
  deriving (Eq, Generic, Show)

newtype Hovering = Hovering
  {hoveredCard :: HandIndex}
  deriving (Eq, Generic, Show)

data Dragging = Dragging
  { draggedCard :: HandIndex,
    dragTarget :: Maybe CardSpot
  }
  deriving (Eq, Show, Generic)

data HandFiddle
  = -- | Card in hand being hovered
    HandHovering HandIndex
  | -- | Card in hand being dragged
    HandDragging HandIndex
  deriving (Eq, Show, Generic)

-- | Type synonym for an application model
data Model = Model
  { board :: Board,
    interaction :: Interaction,
    turn :: Turn,
    uiCards :: [Card UI]
  }
  deriving (Eq, Generic)
