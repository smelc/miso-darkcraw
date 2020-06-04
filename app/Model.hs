{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Model where

import Board
import Card
import GHC.Generics

newtype HandIndex = HandIndex {unHandIndex :: Int}
  deriving (Eq, Show, Generic, Enum)

data Interaction
  = HoverInteraction Hovering
  | DragInteraction Dragging
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
  | -- | Card in hand being dragged to (x, y)
    HandDragging HandIndex
  deriving (Eq, Show, Generic)

-- | Type synonym for an application model
data Model = Model
  { board :: Board,
    uiCards :: [Card UI],
    interaction :: Maybe Interaction
  }

deriving instance (Eq Model)

deriving instance (Generic Model)
