{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Model where

import qualified Art
import Board
import Campaign
import Card
import Cinema (TimedFrame)
import qualified Constants (Difficulty (..))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Text as Text
import qualified Data.Vector as V
import GHC.Generics
import qualified Game (Animation (..), Target)
import Nat
import ServerMessages
import qualified SharedModel as Shared
import Spots hiding (Card)
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

-- | The model of the gaming page. If you add a field, think about
-- extending the 'Show' instance below.
data Game = Game
  { -- | Part of the model shared among all pages
    shared :: Shared.Model,
    -- | The core part of the model
    board :: Board 'Core,
    -- | The game's difficulty
    difficulty :: Constants.Difficulty,
    -- | What user interaction is going on
    interaction :: Interaction Game.Target,
    -- | The list of games to play
    journey :: Campaign.Journey,
    -- | The current level
    level :: Campaign.Level,
    -- | Where the player plays
    playingPlayer :: Spots.Player,
    -- | The deck of 'playingPlayer'
    playingPlayerDeck :: [Card.ID],
    -- | The current turn
    turn :: Turn,
    -- | Whether interactions are possible right now
    uiAvail :: Bool,
    -- | Animations to perform next
    anims :: Board 'UI,
    -- | Animation unrelated to 'Board'
    anim :: Game.Animation
  }
  deriving (Eq, Generic)

instance Show Game where
  show Game {..} =
    "{ gameShared = omitted\n"
      ++ unlines [Art.toASCII board, f "interaction" interaction, f "journey" journey, f "playingPlayer" playingPlayer, f "turn" turn, f "anims" anims]
      ++ "\n}"
    where
      f s x = "  " ++ s ++ " = " ++ show x

-- This implementation will be wrong once volatile cards are generated
-- during a match. When this happen, the player's deck will have to be
-- carried on in GameModel. No big deal.
gameToDeck :: Game -> [Card.ID]
gameToDeck Game {..} =
  inPlace' ++ inHand ++ stack ++ discarded
  where
    PlayerPart {..} = Board.toPart board playingPlayer
    inPlace' = inPlace & Map.elems & map (\Creature {creatureId, items} -> IDC creatureId items)

endGame :: Game -> Campaign.Outcome -> LootModel
endGame Game {board, level, playingPlayer = pSpot, playingPlayerDeck = deck, shared} outcome =
  case Campaign.succ level of
    Nothing -> error "You've finished the game!" -- Not really a nice end for now
    Just next -> LootModel {..}
  where
    nbRewards = 1 -- Change this?
    rewards =
      zip
        (Campaign.loot (Just $ Shared.getStdGen shared) outcome level team)
        (repeat NotPicked)
    team = Board.toPart board pSpot & Board.team

-- | Function for debugging only. Used to
-- make the game start directly on the 'LootView'.
unsafeLootModel :: WelcomeModel -> Model
unsafeLootModel WelcomeModel {shared} =
  LootModel' $ LootModel {..}
  where
    nbRewards = 1
    team = Human
    rewards = zip (getRewards team Campaign.Level0) $ repeat NotPicked
    getRewards team level = Campaign.loot (Just $ Shared.getStdGen shared) Campaign.Win level team
    next = Campaign.Level1
    deck =
      Shared.getInitialDeck shared team
        & map Card.cardToIdentifier

-- | Function for debugging only. Used to
-- make the game start directly on the 'GameView'. Similar to a function
-- in 'Update' (but we don't want to grow 'Update' if we can avoid).
unsafeGameModel :: WelcomeModel -> Model
unsafeGameModel WelcomeModel {shared} =
  GameModel' $ Game {..}
  where
    anim = Game.NoAnimation
    anims = mempty
    journey = Campaign.mkJourney team
    team = Human
    teams = Teams Undead team
    teams' = teams <&> (\t -> (t, Shared.getInitialDeck shared t))
    turn = Turn.initial
    level = Campaign.Level0
    difficulty = Constants.Easy
    interaction = NoInteraction
    playingPlayer = startingPlayerSpot
    (_, board) = Board.initial shared teams'
    playingPlayerDeck =
      toData startingPlayerSpot teams'
        & snd
        & map Card.cardToIdentifier
    uiAvail = True

-- | Whether it's the turn of the playing player, i.e. neither the AI turn
-- | nor the turn of the other player if in multiplayer.
isPlayerTurn :: Game -> Bool
isPlayerTurn Game {playingPlayer, turn} =
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
    shared :: Shared.Model,
    -- | Keys currently down
    keysDown :: Set Int
  }
  deriving (Eq, Generic, Show)

data SinglePlayerLobbyModel = SinglePlayerLobbyModel
  { -- | The chosen team
    singlePlayerLobbyTeam :: Maybe Team,
    -- | Part of the model shared among all pages
    singlePlayerLobbyShared :: Shared.Model
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

data Deck = Deck
  { -- | The deck to show
    deck :: [Card 'Core],
    -- | The model to use when closing the deck view
    deckBack :: Model,
    -- | To which player 'deckBack' belongs
    player :: Spots.Player,
    -- | To which team the deck being shown belongs
    team :: Team,
    -- | Part of the model shared among all pages
    shared :: Shared.Model
  }
  deriving (Eq, Generic, Show)

-- | Whether a card has been picked or not in the 'LootView'
data Picked
  = -- | Reward has been chosen
    Picked
  | -- | Reward has not been chosen
    NotPicked
  deriving (Eq, Generic, Ord, Show)

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
    shared :: Shared.Model,
    -- | The possible rewards, and whether they have been picked already
    -- or not.
    rewards :: [(Card.ID, Picked)]
  }
  deriving (Eq, Generic, Show)

-- | The top level model
data Model
  = DeckModel' Deck
  | GameModel' Game
  | LootModel' LootModel
  | SinglePlayerLobbyModel' SinglePlayerLobbyModel
  | WelcomeModel' WelcomeModel
  | MultiPlayerLobbyModel' MultiPlayerLobbyModel
  deriving (Eq, Generic, Show)
