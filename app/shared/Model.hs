{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Model where

import Board
import Card
import Control.Lens
import Data.Generics.Labels
import qualified Data.Text as Text
import GHC.Generics
import Miso.String
import ServerMessages
import Turn (Turn, turnToPlayerSpot)

-- | An interaction happening in the game page
data GameInteraction
  = -- | Hovering over a card in hand TODO smelc rename to HoverHandInteraction
    GameHoverInteraction Hovering
  | -- | Hovering over a card in place
    GameHoverInPlaceInteraction PlayerSpot CardSpot
  | -- | Dragging a card
    GameDragInteraction Dragging
  | GameNoInteraction
  | GameShowErrorInteraction Text.Text
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
    interaction :: GameInteraction,
    -- | Where the player plays
    playingPlayer :: PlayerSpot,
    -- | The current turn
    turn :: Turn,
    -- | Data obtained at load time, that never changes
    gameCards :: [Card UI],
    -- | Animations to perform next
    anims :: Board UI
  }
  deriving (Eq, Generic, Show)

-- | Whether it's the turn of the playing player, i.e. neither the AI turn
-- | nor the turn of the other player if in multiplayer.
isPlayerTurn :: GameModel -> Bool
isPlayerTurn GameModel {playingPlayer, turn} =
  turnToPlayerSpot turn == playingPlayer

data PlayingMode
  = NoPlayingMode
  | MultiPlayer
  | SinglePlayer
  | SinglePlayerTeam Team
  deriving (Eq, Generic, Show)

-- | The model of the welcome page
data WelcomeModel = WelcomeModel
  { playingMode :: PlayingMode,
    welcomeCards :: [Card UI]
  }
  deriving (Eq, Generic, Show)

data MultiPlayerLobbyModel
  = CollectingUserName UserName
  | WaitingForNameSubmission UserName
  | DisplayingUserList UserName [UserName]
  deriving (Eq, Generic, Show)

-- | The top level model, later it will be a disjunction
-- | of the model of each page
data Model
  = GameModel' GameModel
  | WelcomeModel' WelcomeModel
  | MultiPlayerLobbyModel' MultiPlayerLobbyModel
  deriving (Eq, Generic, Show)
