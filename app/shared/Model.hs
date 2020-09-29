{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Model where

import Board
import Card
import Cinema (ActorChange, ActorState, Frame (..), Scene (..), TimedFrame)
import Control.Lens
import Data.Generics.Labels
import Data.Set (Set)
import qualified Data.Text as Text
import GHC.Generics
import Miso.String
import ServerMessages
import SharedModel (SharedModel (..))
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

data SceneModel
  = SceneNotStarted [TimedFrame ActorState]
  | ScenePlaying [TimedFrame ActorState] (TimedFrame ActorState) [TimedFrame ActorState]
  | ScenePausedForDebugging [TimedFrame ActorState] (TimedFrame ActorState) [TimedFrame ActorState]
  | SceneComplete [TimedFrame ActorState]
  deriving (Eq, Generic, Show)

-- | The model of the welcome page
data WelcomeModel = WelcomeModel
  { -- The state of the scene
    welcomeSceneModel :: SceneModel,
    -- | Part of the model shared among all pages
    welcomeShared :: SharedModel,
    -- | Keys currently down
    keysDown :: Set Int
  }
  deriving (Eq, Generic, Show)

data SinglePlayerLobbyModel = SinglePlayerLobbyModel
  { -- | The chosen team
    singlePlayerLobbyTeam :: Maybe Team,
    -- | Part of the model shared among all pages
    singlePlayerLobbyShared :: SharedModel
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
  | InvitingUser UserName [UserName] UserName InvitationActorState
  | Invited UserName [UserName] UserName InvitedActorState
  | GameStarted UserName UserName
  deriving (Eq, Generic, Show)

data InvitationActorState
  = WaitingForUserInvitationAck
  | WaitingForRSVP
  | WaitingForInvitationDropAck
  deriving (Eq, Generic, Show)

data InvitedActorState
  = CollectingUserRSVP
  | WaitingForRejectionAck
  | WaitingForAcceptanceAck
  deriving (Eq, Generic, Show)

data DeckModel = DeckModel
  { -- | The deck to show
    deck :: [Card Core],
    -- | The model to use when closing the deck view
    deckBack :: Model,
    -- | To which player 'deckBack' belongs
    deckPlayer :: PlayerSpot,
    -- | Part of the model shared among all pages
    deckShared :: SharedModel
  }
  deriving (Eq, Generic, Show)

-- | The top level model, later it will be a disjunction
-- | of the model of each page
data Model
  = DeckModel' DeckModel
  | GameModel' GameModel
  | SinglePlayerLobbyModel' SinglePlayerLobbyModel
  | WelcomeModel' WelcomeModel
  | MultiPlayerLobbyModel' MultiPlayerLobbyModel
  deriving (Eq, Generic, Show)
