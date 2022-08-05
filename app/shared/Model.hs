{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Model where

import qualified Art
import qualified Board
import Campaign
import Card
import Cinema (TimedFrame)
import qualified Configuration
import qualified Constants
import qualified Contains
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified Direction
import GHC.Generics
import qualified Game (Animation (..), Target)
import Nat
import qualified Network
import qualified Roads
import ServerMessages
import qualified Shared
import qualified Spots
import qualified Theme
import qualified Turn

-- | Things that can be hovered or selected.
data Box
  = -- Card in hand
    BoxHand Board.HandIndex
  | -- | Player part/card
    BoxTarget Game.Target
  deriving (Eq, Show, Ord)

-- | An interaction happening in the game page. TODO @smelc remove
-- @Interaction@ suffix of constructors and move to new file?
data Interaction
  = -- | Hovering over a card in hand/in place
    HoverInteraction Box
  | -- | Hovering over something while there is a selection. If there
    -- is only hovering, then 'HoverInteraction' is used. If there is
    -- only a selection, then 'SelectionInteraction' is used.
    HoverSelectionInteraction Box Box
  | NoInteraction
  | SelectionInteraction Box
  | ShowErrorInteraction Text.Text
  deriving (Eq, Generic, Show)

-- | The hovering interaction, if any
toHover :: Interaction -> Maybe Box
toHover = \case
  HoverInteraction b -> Just b
  HoverSelectionInteraction b _ -> Just b
  NoInteraction -> Nothing
  SelectionInteraction _ -> Nothing
  ShowErrorInteraction _ -> Nothing

-- | The selection interaction, if any
toSelection :: Interaction -> Maybe Box
toSelection = \case
  HoverInteraction _ -> Nothing
  HoverSelectionInteraction _ ik -> Just ik
  NoInteraction -> Nothing
  SelectionInteraction ik -> Just ik
  ShowErrorInteraction _ -> Nothing

-- | @addHover ik i@ adds the hover interaction @ik@ to @i@
addHover :: Box -> Interaction -> Interaction
addHover b i =
  case i of
    NoInteraction -> HoverInteraction b
    HoverInteraction _ -> HoverInteraction b
    HoverSelectionInteraction _ s -> HoverSelectionInteraction b s
    SelectionInteraction s -> HoverSelectionInteraction b s
    ShowErrorInteraction {} -> HoverInteraction b

-- | @rmHover i@ removes the hover interaction from i
rmHover :: Interaction -> Interaction
rmHover =
  \case
    NoInteraction -> NoInteraction
    HoverInteraction _ -> NoInteraction
    HoverSelectionInteraction _ s -> SelectionInteraction s
    x@(SelectionInteraction _) -> x
    x@(ShowErrorInteraction {}) -> x

-- | @addSelection s i@ adds the selection interaction @s@ to @i@
addSelection :: Box -> Interaction -> Interaction
addSelection b i =
  case i of
    NoInteraction -> SelectionInteraction b
    HoverInteraction hi -> HoverSelectionInteraction hi b
    HoverSelectionInteraction hi _ -> HoverSelectionInteraction hi b
    SelectionInteraction _ -> SelectionInteraction b
    ShowErrorInteraction {} -> SelectionInteraction b

-- | @rmSelection i@ removes the selection interaction from @i@
rmSelection :: Interaction -> Interaction
rmSelection = \case
  NoInteraction -> NoInteraction
  x@(HoverInteraction _) -> x
  HoverSelectionInteraction hi _ -> HoverInteraction hi
  SelectionInteraction _ -> NoInteraction
  x@(ShowErrorInteraction {}) -> x

-- | The model of the gaming page. If you add a field, think about
-- extending the 'Show' instance below.
data Game = Game
  { -- | Part of the model shared among all pages
    shared :: Shared.Model,
    -- | The core part of the model
    board :: Board.T 'Core,
    -- | The game's difficulty
    difficulty :: Constants.Difficulty,
    -- | The encounter where this game is on the world map. The position
    -- of the encounter is within 'player'.
    encounter :: Network.Encounter,
    -- | What user interaction is going on
    interaction :: Interaction,
    -- | The list of games to play in Vanilla edition, @Nothing@ in
    -- legendary edition.
    journey :: Maybe (Campaign.Journey),
    -- | Data of the playing player
    player :: Player Team,
    -- | The rewards of the entire campaign
    rewards :: [Network.Rewards],
    -- | The current turn
    turn :: Turn.T,
    -- | Whether interactions are possible right now
    uiAvail :: Bool,
    -- | Animations to perform next
    anims :: Board.T 'UI,
    -- | Animation unrelated to 'Board'
    anim :: Game.Animation
  }
  deriving (Eq, Generic)

-- | An instance of 'Model.Game' that is ready for starting a game
mkInitialGame ::
  Shared.Model ->
  Constants.Difficulty ->
  -- | Encounters done already
  Map.Map Direction.Coord Network.Encounter ->
  -- | The location of the current encounter and the encounter itself
  (Direction.Coord, Network.Encounter) ->
  Maybe Journey ->
  -- | The teams
  Board.Teams (Team, [Card 'Core]) ->
  Model.Game
mkInitialGame shared difficulty past (position, encounter) journey teams =
  Model.Game {..}
  where
    (_, board) = Board.initial shared teams
    interaction = NoInteraction
    player = Model.Player {past, position, pSpot = playingPlayer, pDeck, pTeam = team}
    playingPlayer = Spots.startingPlayerSpot
    rewards = (Map.lookup team Network.rewards & fromMaybe [])
    team = Board.toData Spots.startingPlayerSpot teams & fst
    pDeck = Board.toData playingPlayer teams & snd & map Card.cardToIdentifier
    turn = Turn.initial
    anims = mempty
    anim = Game.NoAnimation
    uiAvail = True

-- | A smart constructor for 'Model.World'
mkWorld ::
  Shared.Model ->
  Map.Map Direction.Coord Network.Encounter ->
  Bool ->
  Model.Player (Maybe Team) ->
  (Nat, Nat) ->
  Direction.Coord ->
  Model.World
mkWorld shared encounters moved player size position =
  World {..}
  where
    fade = if Configuration.isDev then Constants.DontFade else Constants.FadeIn
    topology = Network.mkTopology $ concat Roads.points
    topLeft = mkTopLeft position

mkTopLeft :: Direction.Coord -> Direction.Coord
mkTopLeft (Direction.Coord (x, y)) = (Direction.Coord (x - 11, y - 25))

instance Show Game where
  show Game {..} =
    "{ gameShared = omitted\n"
      ++ unlines [Art.toASCII board, f "encounter" encounter, f "interaction" interaction, f "journey" journey, f "playingPlayer" player, f "turn" turn, f "anims" anims]
      ++ "\n}"
    where
      f s x = "  " ++ s ++ " = " ++ show x

instance Contains.Contains Game Turn.T where
  to = turn

-- This implementation will be wrong once volatile cards are generated
-- during a match. When this happen, the player's deck will have to be
-- carried on in GameModel. No big deal.
gameToDeck :: Game -> [Card.ID]
gameToDeck Game {..} =
  inPlace' ++ inHand ++ stack ++ discarded
  where
    Model.Player {pSpot} = player
    Board.PlayerPart {..} = Board.toPart board pSpot
    inPlace' = inPlace & Map.elems & map (\Creature {creatureId, items} -> IDC creatureId items)

gameToLoot :: Model.Game -> Campaign.Outcome -> Model.Loot
gameToLoot Game {encounter, player, rewards, shared} _outcome =
  Model.Loot {player = player', rewards = loot, ..}
  where
    (nbRewards, loot) =
      case rewards of
        [] -> (0, [])
        Network.Rewards x rs : _ -> (x, zip rs (repeat Model.NotPicked))
    Model.Player {past, position} = player
    past' = uncurry Map.insert (position, encounter) past
    player' = player {past = past'}

-- | Function for debugging only. Used to
-- make the game start directly on the 'LootView'.
unsafeLootModel :: Model.Welcome -> Model
unsafeLootModel Model.Welcome {shared} =
  Loot' $ Model.Loot {..}
  where
    nbRewards = 1
    player = Model.Player {past, pDeck = deck, position, pSpot = Spots.startingPlayerSpot, pTeam = team}
    (past, position, team) = (mempty, Direction.Coord (0, 0), Human)
    rewards = [(Card.IDI Card.BowOfGaia, Model.NotPicked)]
    -- zip ( & fromMaybe []) $ repeat NotPicked
    deck =
      Shared.getInitialDeck shared team
        & map Card.cardToIdentifier

-- | Function for debugging only. Used to
-- make the game start directly on the 'GameView'. Similar to a function
-- in 'Update' (but we don't want to grow 'Update' if we can avoid).
unsafeGameModel :: Model.Welcome -> Model
unsafeGameModel Model.Welcome {shared} =
  Game' $ Game {..}
  where
    anim = Game.NoAnimation
    anims = mempty
    journey = Just (Campaign.mkJourney team)
    (position, encounter) = (Direction.Coord (0, 0), Network.Fight opponent Theme.Forest)
    past = mempty
    rewards = Map.lookup team Network.rewards & fromMaybe []
    (team, opponent) = (Human, Undead)
    teams = Board.Teams opponent team
    teams' = teams <&> (\t -> (t, Shared.getInitialDeck shared t))
    turn = Turn.initial
    difficulty = Constants.Easy
    interaction = NoInteraction
    playingPlayer = Spots.startingPlayerSpot
    (_, board) = Board.initial shared teams'
    player = Model.Player {pSpot = playingPlayer, pTeam = team, ..}
    pDeck = Board.toData playingPlayer teams' & snd & map Card.cardToIdentifier
    uiAvail = True

-- | Whether it's the turn of the playing player, i.e. neither the AI turn
-- | nor the turn of the other player if in multiplayer.
isPlayerTurn :: Game -> Bool
isPlayerTurn Game {player, turn} =
  Turn.toPlayerSpot turn == (Model.pSpot player)

-- | Data of the playing player
data Player a = Player
  { -- | Encounters done already
    past :: Map.Map Direction.Coord Network.Encounter,
    -- | The deck
    pDeck :: [Card.ID],
    -- | The absolute position of the character, in number of cells
    position :: Direction.Coord,
    -- | Where the player plays
    pSpot :: Spots.Player,
    -- | The player's team. Either instantiated by @Maybe Team@ before the team
    -- has been picked in 'World'. Then instantiated by @Team@.
    pTeam :: a
  }
  deriving (Eq, Functor, Generic, Show)

instance Contains.With (Player a) Direction.Coord where
  with p position = p {position}

-- | The model of the world page. If you add a field, consider
-- extending the Show and Eq instances below.
data World = forall a.
  Network.Network a =>
  World
  { -- | Possible encounters
    encounters :: Map.Map Direction.Coord Network.Encounter,
    -- | The fading (if any)
    fade :: Constants.Fade,
    -- | Whether a move was done already
    moved :: Bool,
    -- | Data of the playing player
    player :: Player (Maybe Team),
    -- | Part of the model shared among all pages
    shared :: Shared.Model,
    -- | The size of the view, in number of cells (width, height)
    size :: (Nat, Nat),
    -- | Coordinate of the most top left visible cell, in number of cells
    topLeft :: Direction.Coord,
    topology :: a
  }

instance Eq World where
  (==)
    (World {player = player1, size = sz1, topLeft = tl1})
    (World {player = player2, size = sz2, topLeft = tl2}) =
      (position player1) == (position player2) && sz1 == sz2 && tl1 == tl2

instance Show World where
  show World {player, size, topLeft} =
    concat $
      intersperse
        " "
        ["position:", show (position player), " size:", show size, "topLeft:", show topLeft]

instance Contains.With World Direction.Coord where
  with w@World {player = p} c = w {player = p `Contains.with` c}

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
data Welcome = Welcome
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
    -- | The current turn
    turn :: Turn.T,
    -- | Part of the model shared among all pages
    shared :: Shared.Model
  }
  deriving (Eq, Generic, Show)

instance Contains.Contains Deck Turn.T where
  to = turn

-- | Whether a card has been picked or not in the 'LootView'
data Picked
  = -- | Reward has been chosen
    Picked
  | -- | Reward has not been chosen
    NotPicked
  deriving (Eq, Generic, Ord, Show)

data Loot = Loot
  { -- | The number of rewards to be picked from 'rewards'
    nbRewards :: Nat,
    -- | Data of the playing player
    player :: Player Team,
    -- | Part of the model shared among all pages
    shared :: Shared.Model,
    -- | The possible rewards, and whether they have been picked already
    -- or not.
    rewards :: [(Card.ID, Picked)]
  }
  deriving (Eq, Generic, Show)

-- | The top level model. TODO @smelc introduce a "View" typeclass
-- and use "Model = forall a. View a => Model a"? But I would lose
-- pattern matching, hum?
data Model
  = Deck' Model.Deck
  | Game' Model.Game
  | Loot' Model.Loot
  | SinglePlayerLobbyModel' SinglePlayerLobbyModel
  | Welcome' Model.Welcome
  | MultiPlayerLobbyModel' MultiPlayerLobbyModel
  | World' Model.World
  deriving (Eq, Generic, Show)
