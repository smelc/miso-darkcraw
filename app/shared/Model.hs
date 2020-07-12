{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model where

import Board
import Card
import qualified Data.Text as Text
import GHC.Generics
import Turn (Turn)

newtype HandIndex = HandIndex {unHandIndex :: Int}
  deriving (Eq, Show, Generic, Enum)

data Interaction
  = -- | Hovering over a card in hand TODO smelc rename to HoverHandInteraction
    HoverInteraction Hovering
  | -- | Hovering over a card in place
    HoverInPlaceInteraction PlayerSpot CardSpot
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

-- | The model of the gaming page
data GameModel = GameModel
  { -- | The core part of the model
    board :: Board Core,
    -- | What user interaction is going on
    interaction :: Interaction,
    -- | The current turn
    turn :: Turn,
    -- | Data obtained at load time, that never changes
    uiCards :: [Card UI],
    -- | Animations to perform next
    anims :: Board UI
  }
  deriving (Eq, Generic)

-- | The top level model, later it will be a disjunction
-- | of the model of each page
newtype Model = GameModel' GameModel
  deriving (Eq, Generic)