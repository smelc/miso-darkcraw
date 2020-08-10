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
import System.Random
import Turn (Turn, turnToPlayerSpot)

-- | An interaction happening in the game page
data GameInteraction
  = -- | Hovering over a card in hand
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
  { -- | Part of the model shared among all pages
    gameShared :: SharedModel,
    -- | The core part of the model
    board :: Board Core,
    -- | What user interaction is going on
    interaction :: GameInteraction,
    -- | Where the player plays
    playingPlayer :: PlayerSpot,
    -- | The current turn
    turn :: Turn,
    -- | Animations to perform next
    anims :: Board UI
  }
  deriving (Eq, Generic, Show)

-- | The part of the model that is likely to be used by all pages
-- | i.e. all possible models
data SharedModel = SharedModel
  -- If you add a field, you ***MUST*** extend the class ToExpr in Update.hs
  { -- | Data obtained at load time, that never changes
    sharedCards :: [Card UI],
    -- | RNG obtained at load time, to be user whenever randomness is needed
    sharedStdGen :: StdGen
  }
  deriving (Generic, Show)

instance Eq SharedModel where
  SharedModel {sharedCards = sharedCards1} == SharedModel {sharedCards = sharedCards2} =
    sharedCards1 == sharedCards2

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
  { -- | Part of the model shared among all pages
    welcomeShared :: SharedModel,
    playingMode :: PlayingMode
  }
  deriving (Eq, Generic, Show)

data MultiPlayerLobbyError
  = InvitationCancelledError UserName
  | InvitationRejectedError UserName
  | UserBusyError UserName
  deriving (Eq, Generic, Show)

data MultiPlayerLobbyModel
  = CollectingUserName UserName
  | WaitingForNameSubmission UserName
  | DisplayingUserList (Maybe MultiPlayerLobbyError) UserName [UserName]
  | InvitingUser UserName [UserName] UserName InvitationState
  | Invited UserName [UserName] UserName InvitedState
  | GameStarted UserName UserName
  deriving (Eq, Generic, Show)

data InvitationState
  = WaitingForUserInvitationAck
  | WaitingForRSVP
  | WaitingForInvitationDropAck
  deriving (Eq, Generic, Show)

data InvitedState
  = CollectingUserRSVP
  | WaitingForRejectionAck
  | WaitingForAcceptanceAck
  deriving (Eq, Generic, Show)

-- | The top level model, later it will be a disjunction
-- | of the model of each page
data Model
  = GameModel' GameModel
  | WelcomeModel' WelcomeModel
  | MultiPlayerLobbyModel' MultiPlayerLobbyModel
  deriving (Eq, Generic, Show)
