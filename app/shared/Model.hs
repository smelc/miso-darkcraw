{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Model where

import AI (Difficulty)
import Board
import Campaign
import Card
import Cinema (TimedFrame)
import Data.Function ((&))
import Data.Generics.Labels ()
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Text as Text
import qualified Data.Vector as V
import GHC.Generics
import qualified Game (Target)
import Nat
import ServerMessages
import SharedModel (SharedModel, getInitialDeck)
import Turn (Turn)
import qualified Turn

-- | An interaction happening in the game page
data Interaction a
  = -- | Hovering over a card in hand
    HoverInteraction Hovering
  | -- | Hovering over a target
    HoverInPlaceInteraction a
  | -- | Dragging a card
    DragInteraction (Dragging a)
  | NoInteraction
  | ShowErrorInteraction Text.Text
  deriving (Eq, Generic, Show)

newtype Hovering = Hovering
  {hoveredCard :: HandIndex}
  deriving (Eq, Generic, Show)

data Dragging a = Dragging
  { draggedCard :: HandIndex,
    dragTarget :: Maybe a
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
    shared :: SharedModel,
    -- | The core part of the model
    board :: Board 'Core,
    -- | The game's difficulty
    difficulty :: Difficulty,
    -- | What user interaction is going on
    interaction :: Interaction Game.Target,
    -- | The current level
    level :: Campaign.Level,
    -- | Where the player plays
    playingPlayer :: PlayerSpot,
    -- | The deck of 'playingPlayer'
    playingPlayerDeck :: [Card.ID],
    -- | The current turn
    turn :: Turn,
    -- | Animations to perform next
    anims :: Board 'UI
  }
  deriving (Eq, Generic)

data BuildModel = BuildModel
  { -- | Part of the model shared among all pages
    shared :: SharedModel,
    -- | The actual deck
    deck :: [Card.ID],
    -- | The playing player's spot and team
    buildPlayer :: (PlayerSpot, Team),
    -- | The number of cards that can still be drawn from 'hand'
    free :: Int,
    -- | Cards to augment the deck
    hand :: [Card.ID]
  }
  deriving (Eq, Generic, Show)

gameToBuild :: GameModel -> BuildModel
gameToBuild gm@GameModel {..} =
  BuildModel {..}
  where
    deck = gameToDeck gm
    buildPlayer = (playingPlayer, Board.toPart board playingPlayer & Board.team)
    free = 0 -- for the moment
    hand = [] -- for the moment

-- This implementation will be wrong once volatile cards are generated
-- during a match. When this happen, the player's deck will have to be
-- carried on in GameModel. No big deal.
gameToDeck :: GameModel -> [Card.ID]
gameToDeck GameModel {..} =
  inPlace' ++ inHand ++ stack ++ discarded
  where
    PlayerPart {..} = Board.toPart board playingPlayer
    inPlace' = inPlace & Map.elems & map (\Creature {creatureId, items} -> IDC creatureId items)

endGame :: GameModel -> Campaign.Outcome -> Model
endGame
  GameModel
    { board,
      level,
      playingPlayer = pSpot,
      playingPlayerDeck = deck,
      ..
    }
  outcome =
    case Campaign.succ level of
      Nothing -> error "You've finished the game!" -- Not really a nice end for now
      Just next -> LootModel' $ LootModel {..}
    where
      nbRewards = 1
      rewards = zip (Campaign.loot outcome level team) $ repeat NotPicked
      team = Board.toPart board pSpot & Board.team

-- | Function for debugging only. To be deleted at some point.
unsafeLootModel :: WelcomeModel -> Model
unsafeLootModel WelcomeModel {shared} =
  LootModel' $ LootModel {..}
  where
    nbRewards = 1
    team = Human
    rewards = zip (Campaign.loot Campaign.Win Campaign.Level0 team) $ repeat NotPicked
    next = Campaign.Level1
    deck =
      SharedModel.getInitialDeck shared team
        & map Card.cardToIdentifier

instance Show GameModel where
  show GameModel {..} =
    "{ gameShared = omitted\n"
      ++ unlines [Board.toASCII board, f "interaction" interaction, f "playingPlayer" playingPlayer, f "turn" turn, f "anims" anims]
      ++ "\n}"
    where
      f s x = "  " ++ s ++ " = " ++ show x

-- | Whether it's the turn of the playing player, i.e. neither the AI turn
-- | nor the turn of the other player if in multiplayer.
isPlayerTurn :: GameModel -> Bool
isPlayerTurn GameModel {playingPlayer, turn} =
  Turn.toPlayerSpot turn == playingPlayer

data PlayingMode
  = NoPlayingMode
  | MultiPlayer
  | SinglePlayer
  | SinglePlayerTeam Team
  deriving (Eq, Generic, Show)

type TimedFrames = V.Vector TimedFrame

data SceneModel
  = SceneNotStarted TimedFrames
  | ScenePlaying TimedFrames Int
  | ScenePausedForDebugging TimedFrames Int
  | SceneComplete TimedFrames
  deriving (Eq, Generic, Show)

-- | The model of the welcome page
data WelcomeModel = WelcomeModel
  { -- The state of the scene
    sceneModel :: SceneModel,
    -- | Part of the model shared among all pages
    shared :: SharedModel,
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
    deck :: [Card 'Core],
    -- | The model to use when closing the deck view
    deckBack :: Model,
    -- | To which player 'deckBack' belongs
    player :: PlayerSpot,
    -- | To which team the deck being shown belongs
    team :: Team,
    -- | Part of the model shared among all pages
    shared :: SharedModel
  }
  deriving (Eq, Generic, Show)

-- | Whether a card has been picked or not in the 'LootView'
data Picked
  = -- | Reward has been chosen
    Picked
  | -- | Reward has not been chosen
    NotPicked
  deriving (Eq, Generic, Show)

data LootModel = LootModel
  { -- | The number of rewards to be picked from 'rewards'
    nbRewards :: Nat,
    -- | The next level
    next :: Level,
    -- | The deck of the playing player
    deck :: [Card.ID],
    -- | To which team the deck being shown belongs
    team :: Team,
    -- | Part of the model shared among all pages
    shared :: SharedModel,
    -- | The possible rewards, and whether they have been picked already
    -- or not.
    rewards :: [(Card.ID, Picked)]
  }
  deriving (Eq, Generic, Show)

-- | The top level model
data Model
  = BuildModel' BuildModel
  | DeckModel' DeckModel
  | GameModel' GameModel
  | LootModel' LootModel
  | SinglePlayerLobbyModel' SinglePlayerLobbyModel
  | WelcomeModel' WelcomeModel
  | MultiPlayerLobbyModel' MultiPlayerLobbyModel
  deriving (Eq, Generic, Show)
