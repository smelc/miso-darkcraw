{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Update where

import qualified AI
import qualified Board
import qualified Campaign
import Card
import Cinema
import qualified Command
import qualified Constants
import qualified Constants (Difficulty)
import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad.Except
import Data.Foldable (asum, toList)
import Data.List.Index (setAt)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe (isJust, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Data.TreeDiff
import qualified Data.Vector as V
import Debug.Trace
import GHC.Base (assert)
import qualified Game
import qualified Mana
import Miso
import Miso.String (MisoString, fromMisoString)
import Model
import Move (Move, NextSched)
import qualified Move
import Movie (welcomeMovie)
import Nat
import ServerMessages
import qualified Shared
import qualified Spots hiding (Card)
import Text.Pretty.Simple
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Turn

data MultiPlayerLobbyAction
  = LobbyUpdateUsername MisoString
  | LobbySubmitUsername
  | LobbyInviteUser UserName
  | CancelInvitationClicked
  | AcceptInvitationClicked
  | RejectInvitationClicked
  | LobbyServerMessage OutMessage
  deriving (Show, Eq)

data SinglePlayerLobbyAction
  = LobbySelectTeam (Maybe Team)
  deriving (Show, Eq)

-- | Actions internal to 'LootView'
data LootAction
  = -- | Select a reward. The index is in the list of available rewards.
    Pick Nat
  | -- | Unselect a reward. The index is in the list of available rewards.
    Unpick Nat
  deriving (Eq, Show)

-- | To which page to go to, from the welcome page
data WelcomeDestination
  = MultiPlayerDestination
  | SinglePlayerDestination
  deriving (Eq, Show)

type DeckViewInput = [Card 'Core]

-- | Sum type for application events. If drop stuff doesn't work
-- | think whether it's affected by https://github.com/dmjio/miso/issues/478
data Action
  = -- | Leave 'DeckView', go to another view
    DeckBack
  | -- | Leave a view, go to 'DeckView'
    DeckGo DeckViewInput
  | GameAction' Move
  | -- | Actions internal to 'LootView'
    LootAction' LootAction
  | -- | Go to 'LootView'
    LootGo LootModel
  | NoOp
  | MultiPlayerLobbyAction' MultiPlayerLobbyAction
  | SayHelloWorld
  | SinglePlayerLobbyAction' SinglePlayerLobbyAction
  | -- Leave 'SinglePlayerView', back to 'WelcomeView'
    SinglePlayerBack
  | -- Leave 'SinglePlayerView', start game
    SinglePlayerGo
  | SceneAction' SceneAction
  | Keyboard (Set Int)
  | -- Leave 'WelcomeView', go to 'MultiPlayerView' or 'SinglePlayerView'
    WelcomeGo WelcomeDestination
  deriving (Show, Eq)

data SceneAction
  = StepScene
  | PauseOrResumeSceneForDebugging
  | JumpToFrameForDebugging Int
  deriving (Show, Eq)

logUpdates :: (Monad m, Eq a, ToExpr a) => (Action -> a -> m a) -> Action -> a -> m a
logUpdates update NoOp model = update NoOp model
logUpdates update action model = do
  model' <- update action model
  return $
    trace
      ("--------\n" ++ show action ++ "\n" ++ diff model model')
      model'
  where
    diff model model'
      | model == model' = "no diff"
      | otherwise = prettyDiff (ediff model model')
    prettyDiff edits = displayS (renderPretty 0.4 80 (ansiWlEditExprCompact edits)) ""

-- | Class defining the behavior of updating 'm' w.r.t. 'Interaction'
-- and 'DnDAction'
class Interactable m t mseq | m -> t, m -> mseq where
  considerAction :: m -> Move.DnDAction t -> Bool

  -- | What to do when dropping card at given index, on the given 't' target
  -- The spot indicates the player whose card is being played
  drop ::
    MonadError Text.Text n =>
    m ->
    Spots.Player ->
    Board.HandIndex ->
    t ->
    n mseq

  getPlayingPlayer :: m -> Spots.Player

  -- | When to stop a dropping action
  stopWrongDrop :: m -> Bool

  -- | The default behavior
  updateDefault :: m -> Interaction t -> mseq

  -- | How to update the abstract state 'm' with the given 'Interaction'
  withInteraction :: m -> Interaction t -> m

act ::
  MonadError Text.Text n =>
  forall m t mseq.
  Interactable m t mseq =>
  m ->
  Move.DnDAction t ->
  Interaction t ->
  n mseq
act m a i =
  case (considerAction m a, a, i) of
    (False, _, _) -> pure $ updateDefault m NoInteraction
    (_, Move.DragEnd, _) -> pure $ updateDefault m NoInteraction
    (_, Move.DragStart j, _) ->
      pure $ updateDefault m $ DragInteraction $ Dragging j Nothing
    (_, Move.Drop, DragInteraction Dragging {draggedCard, dragTarget = Just dragTarget}) ->
      Update.drop
        m
        pSpot
        draggedCard
        dragTarget
      where
        pSpot = getPlayingPlayer m
    (_, Move.Drop, _) | stopWrongDrop m -> pure $ updateDefault m NoInteraction
    -- DragEnter cannot create a DragInteraction if there's none yet, we don't
    -- want to keep track of drag targets if a drag action did not start yet
    (_, Move.DragEnter t, DragInteraction dragging) ->
      pure $ updateDefault m $ DragInteraction $ dragging {dragTarget = Just t}
    (_, Move.DragLeave _, DragInteraction dragging) ->
      pure $ updateDefault m $ DragInteraction $ dragging {dragTarget = Nothing}
    _ -> pure $ updateDefault m i

instance Interactable Model.Game Game.Target (Model.Game, NextSched) where
  considerAction m@Model.Game {uiAvail} a =
    case a of
      Move.DragEnd -> True
      Move.DragStart _ | uiAvail && isPlayerTurn m -> True
      Move.DragStart _ -> False
      Move.Drop -> True
      Move.DragEnter _ -> isPlayerTurn m
      Move.DragLeave _ -> isPlayerTurn m

  drop m pSpot idx target = playOne m $ Game.PEvent $ Game.Place pSpot target idx

  getPlayingPlayer Model.Game {turn} = Turn.toPlayerSpot turn

  stopWrongDrop m = isPlayerTurn m

  updateDefault m i = (withInteraction m i, Nothing)

  withInteraction g i = g {interaction = i}

singleton :: a -> NonEmpty a
singleton x = x NE.:| []

-- | Transforms a 'NextSched' into a value suitable for being
-- passed to 'delayActions'.
nextSchedToMiso :: NextSched -> [(Int, Action)]
nextSchedToMiso ns =
  maybeToList $
    fmap (\(n, ga) -> (Nat.natToInt n & toSecs, GameAction' $ Move.Sched ga)) ns

playOne ::
  MonadError Text.Text m =>
  Model.Game ->
  Game.Event ->
  m (Model.Game, NextSched)
playOne m event =
  updateGameModel m (Move.Sched $ Move.Play event) NoInteraction

-- | We MUST return 'Sched' as the second element (as opposed to
-- 'Game.Event'). This is used for 'GameIncrTurn' for example.
-- The second element must only contain delayed stuff, it's invalid
-- to return a list whose first element has 0. If we did that, we would
-- need to play this event right away, and then we would have new delayed
-- actions to consider which we wouldn't know how to merge with the first ones.
-- If you feel like adding a 0 event, you instead need to play this event
-- right away by doing a recursive call (or use 'playOne')
updateGameModel ::
  MonadError Text.Text m =>
  Model.Game ->
  Move ->
  Interaction Game.Target ->
  m (Model.Game, NextSched)
-- This is the only definition that should care about GameShowErrorInteraction:
updateGameModel m action (ShowErrorInteraction _) =
  updateGameModel m action NoInteraction -- clear error message
updateGameModel m (Move.Sched s) _ =
  -- This is the only definition that should care about Move.Sched
  Move.prodRunOneModel s m
-- Now onto "normal" stuff:
updateGameModel m (Move.DnD a@Move.DragEnd) i = act m a i
updateGameModel m (Move.DnD a@(Move.DragStart _)) i = act m a i
updateGameModel m (Move.DnD a@Move.Drop) i = act m a i
updateGameModel m (Move.DnD a@(Move.DragEnter _)) i = act m a i
updateGameModel m (Move.DnD a@(Move.DragLeave _)) i = act m a i
-- A GamePlayEvent to execute.
-- Hovering in hand cards
updateGameModel m (Move.InHandMouseEnter i) NoInteraction =
  pure $ updateDefault m $ HoverInteraction (Model.InHand i)
updateGameModel m (Move.InHandMouseLeave _) _ =
  pure $ updateDefault m NoInteraction
-- Hovering in place cards
updateGameModel m (Move.InPlaceMouseEnter spots) NoInteraction =
  pure $ updateDefault m $ HoverInteraction (Model.InPlace spots)
updateGameModel m (Move.InPlaceMouseLeave _) _ =
  pure $ updateDefault m NoInteraction
-- Selecting cards.
updateGameModel m (Move.Selection s) NoInteraction =
  pure $ updateDefault m $ SelectionInteraction s
-- Debug cmd
updateGameModel m Move.ExecuteCmd _ =
  pure $
    updateDefault m $
      ShowErrorInteraction "GameExecuteCmd should be handled in updateModel, because it can change page"
updateGameModel m@Model.Game {shared} (Move.UpdateCmd misoStr) i =
  pure $
    updateDefault
      ((m {shared = Shared.withCmd shared (Just $ fromMisoString misoStr)}) :: Model.Game)
      i
-- default
updateGameModel m _ i =
  pure $ updateDefault m i

-- | Update a 'LootModel' according to the input 'LootAction'
updateLootModel :: LootAction -> LootModel -> LootModel
updateLootModel action lm@LootModel {rewards = pairs} =
  lm {rewards}
  where
    rewards =
      case action of
        Pick n -> set n Model.Picked
        Unpick n -> set n Model.NotPicked
    set n value =
      setAt (natToInt n) (card, value) pairs
      where
        card = pairs !! (natToInt n) & fst

updateSinglePlayerLobbyModel ::
  SinglePlayerLobbyAction ->
  SinglePlayerLobbyModel ->
  SinglePlayerLobbyModel
updateSinglePlayerLobbyModel (LobbySelectTeam t) m =
  m {singlePlayerLobbyTeam = t}

-- | Updates a 'MultiPlayerLobbyModel'
updateMultiPlayerLobbyModel :: MultiPlayerLobbyAction -> MultiPlayerLobbyModel -> Effect Action MultiPlayerLobbyModel
updateMultiPlayerLobbyModel
  (LobbyUpdateUsername userName)
  (CollectingUserName _) =
    noEff $ CollectingUserName (fromMisoString userName)
updateMultiPlayerLobbyModel
  LobbySubmitUsername
  (CollectingUserName userName) =
    WaitingForNameSubmission userName <# do
      Miso.send (CreateUser userName)
      return NoOp
updateMultiPlayerLobbyModel
  (LobbyServerMessage UserCreated)
  (WaitingForNameSubmission userName) =
    noEff $ DisplayingUserList Nothing userName []
updateMultiPlayerLobbyModel
  (LobbyServerMessage (NewUserList users))
  (DisplayingUserList merror userName _) =
    noEff $ DisplayingUserList merror userName users
updateMultiPlayerLobbyModel
  (LobbyServerMessage (NewUserList users))
  (InvitingUser me _ user state) =
    noEff $ InvitingUser me users user state
updateMultiPlayerLobbyModel
  (LobbyServerMessage (NewUserList users))
  (Invited me _ user state) =
    noEff $ Invited me users user state
updateMultiPlayerLobbyModel
  (LobbyInviteUser user)
  (DisplayingUserList _ me users) =
    InvitingUser me users user WaitingForUserInvitationAck <# do
      Miso.send (InviteUser user)
      return NoOp
updateMultiPlayerLobbyModel
  (LobbyServerMessage UserBusy)
  (InvitingUser me users user WaitingForUserInvitationAck) =
    noEff $ DisplayingUserList (Just (UserBusyError user)) me users
updateMultiPlayerLobbyModel
  (LobbyServerMessage InvitationSent)
  (InvitingUser me users user WaitingForUserInvitationAck) =
    noEff $ InvitingUser me users user WaitingForRSVP
updateMultiPlayerLobbyModel
  CancelInvitationClicked
  (InvitingUser me users user WaitingForRSVP) =
    InvitingUser me users user WaitingForInvitationDropAck <# do
      Miso.send DropInvitation
      return NoOp
updateMultiPlayerLobbyModel
  (LobbyServerMessage InvitationDropAck)
  (InvitingUser me users _ WaitingForInvitationDropAck) =
    noEff $ DisplayingUserList Nothing me users
updateMultiPlayerLobbyModel
  (LobbyServerMessage InvitationRejected)
  (InvitingUser me users user WaitingForRSVP) =
    noEff $ DisplayingUserList (Just (InvitationRejectedError user)) me users
updateMultiPlayerLobbyModel
  (LobbyServerMessage InvitationAccepted)
  (InvitingUser me _ user WaitingForRSVP) =
    noEff $ GameStarted me user
updateMultiPlayerLobbyModel
  (LobbyServerMessage (IncomingInvitation user))
  (DisplayingUserList _ me users) =
    noEff $ Invited me users user CollectingUserRSVP
updateMultiPlayerLobbyModel
  (LobbyServerMessage IncomingInvitationCancelled)
  (Invited me users user _) =
    noEff $ DisplayingUserList (Just (InvitationCancelledError user)) me users
updateMultiPlayerLobbyModel
  RejectInvitationClicked
  (Invited me users user CollectingUserRSVP) =
    Invited me users user WaitingForRejectionAck <# do
      Miso.send RejectInvitation
      return NoOp
updateMultiPlayerLobbyModel
  (LobbyServerMessage IncomingInvitationRejectionAck)
  (Invited me users _ WaitingForRejectionAck) =
    noEff $ DisplayingUserList Nothing me users
updateMultiPlayerLobbyModel
  AcceptInvitationClicked
  (Invited me users user CollectingUserRSVP) =
    Invited me users user WaitingForAcceptanceAck <# do
      Miso.send AcceptInvitation
      return NoOp
updateMultiPlayerLobbyModel
  (LobbyServerMessage IncomingInvitationAcceptanceAck)
  (Invited me _ user WaitingForAcceptanceAck) =
    noEff $ GameStarted me user
updateMultiPlayerLobbyModel _ m =
  noEff m

-- | Function courtesy of @dmjio! Input type should be 'Int'. 'Nat' might
-- be too small (see 'tenthToSecs').
delayActions :: m -> [(Int, Action)] -> Effect Action m
delayActions m actions =
  Effect
    m
    [ \sink -> do
        liftIO $ threadDelay i
        liftIO (sink action)
      | (i, action) <- actions
    ]

toSceneModel :: Scene () -> SceneModel
toSceneModel scene = SceneNotStarted (V.fromList (Cinema.render scene))

-- | Seconds to scheduling delay
toSecs :: Int -> Int
toSecs x = x * 1000000

-- | Tenth of seconds to scheduling delay
tenthToSecs :: Int -> Int
tenthToSecs x = x * 100000

updateSceneModel :: SceneAction -> SceneModel -> Effect Action SceneModel
updateSceneModel StepScene sceneModel =
  case sceneModel of
    SceneNotStarted frames
      | V.null frames -> noEff $ SceneComplete frames
      | otherwise -> playFrame frames 0
    ScenePlaying frames i
      | i < length frames - 1 -> playFrame frames (i + 1)
      | otherwise -> noEff $ SceneComplete frames
    completeOrPaused -> noEff completeOrPaused
  where
    playFrame frames i =
      let TimedFrame {duration} = frames V.! i
       in delayActions
            (ScenePlaying frames i)
            [(tenthToSecs duration, SceneAction' StepScene)]
updateSceneModel PauseOrResumeSceneForDebugging sceneModel =
  case sceneModel of
    SceneNotStarted frames ->
      noEff $ ScenePausedForDebugging frames 0
    ScenePlaying frames i ->
      noEff $ ScenePausedForDebugging frames i
    SceneComplete frames ->
      noEff $ ScenePausedForDebugging frames (length frames - 1)
    ScenePausedForDebugging frames i ->
      ScenePlaying frames i <# return (SceneAction' StepScene)
updateSceneModel (JumpToFrameForDebugging i) sceneModel =
  case sceneModel of
    SceneNotStarted frames
      | indexWithinBounds frames ->
          noEff $ ScenePausedForDebugging frames i
    SceneComplete frames
      | indexWithinBounds frames ->
          noEff $ ScenePausedForDebugging frames i
    ScenePausedForDebugging frames _
      | indexWithinBounds frames ->
          noEff $ ScenePausedForDebugging frames i
    anyOtherState ->
      noEff anyOtherState
  where
    indexWithinBounds frames = i >= 0 && i < length frames

-- | Updates model, optionally introduces side effects
-- | This function delegates to the various specialized functions
-- | and is the only one to handle page changes. A page change event
-- | is one that takes a model from a page and returns a model of another page.
-- | Such an event cannot be dealt with a specialized function
-- | (SpecializedAction -> SpecializedModel -> SpecializedModel),
-- | it needs to be in `Action -> Model -> Model`.
updateModel :: Action -> Model -> Effect Action Model
-- Generic actions
updateModel NoOp m = noEff m
updateModel SayHelloWorld m =
  -- Say hello and start scene stepping
  m <# do consoleLog "miso-darkcraw says hello" >> pure (SceneAction' StepScene)
-- Actions that change the page
-- Leave 'DeckView'
updateModel DeckBack (DeckModel' Model.Deck {..}) =
  noEff deckBack
-- Leave 'GameView', go to 'DeckView'
updateModel (DeckGo deck) m@(GameModel' Model.Game {..}) =
  noEff $ DeckModel' $ Model.Deck deck m playingPlayer t turn shared
  where
    t = Board.toPart board playingPlayer & Board.team
-- Go to 'LootView'
updateModel (LootGo model) _ =
  noEff $ LootModel' model
-- Schedule leaving 'GameView', to go to 'LootView'
updateModel _ m@(GameModel' gm@Model.Game {board, level, playingPlayer, turn})
  | (Turn.next turn & Turn.toNat) > Constants.nbTurns =
      case Campaign.succ level of
        Nothing -> noEff m -- TODO, go to global victory view
        Just _ ->
          delayActions m' [(toSecs 1, LootGo $ Model.endGame gm outcome)]
          where
            m' = GameModel' gm {anim = Game.Fadeout}
            part = Board.toPart board playingPlayer
            (score, enemy) =
              (Board.score part, Board.toPart board (Spots.other playingPlayer) & Board.score)
            outcome :: Campaign.Outcome =
              case compare score enemy of
                LT -> Campaign.Loss
                EQ -> Campaign.Draw
                GT -> Campaign.Win
-- Leave 'GameView' (maybe)
updateModel (GameAction' Move.ExecuteCmd) (GameModel' gm@Model.Game {board, shared, turn, playingPlayer})
  | Shared.cmd shared & isJust =
      let cmdStr = Shared.cmd shared
       in case cmdStr <&> Mana.read & join of
            Nothing ->
              let errMsg = "Unrecognized command: " ++ show cmdStr
               in noEff $ GameModel' $ gm {interaction = ShowErrorInteraction $ Text.pack errMsg}
            Just (Command.AIPlay pSpot) ->
              case Game.playAll shared $ Game.Playable board events t of
                Left errMsg -> noEff $ GameModel' $ gm {interaction = ShowErrorInteraction errMsg}
                Right (Game.Result {board = board'}) -> withBoard board'
              where
                t = Turn.setSpot pSpot turn
                events = AI.play Constants.Hard shared board pSpot t & map Game.PEvent
            Just (Command.Assassins pSpot) ->
              playEvent Game.ApplyAssassins pSpot
            Just (Command.CreateForest pSpot) ->
              playEvent Game.ApplyCreateForest pSpot
            Just (Command.EndGame outcome) ->
              noEff $ LootModel' $ Model.endGame gm outcome
            Just (Command.FillTheFrontline pSpot) ->
              playEvent Game.FillTheFrontline pSpot
            Just (Command.Gimme cid) ->
              withBoard $ Board.mappk @'Board.Hand (++ [cid]) playingPlayer board
            Just (Command.GimmeMana) ->
              withBoard $ Board.mappk @'Board.Mana ((+) 1) playingPlayer board
            Just (Command.Growth pSpot) ->
              playEvent Game.ApplyGrowth pSpot
            Just (Command.HailToTheKing pSpot) ->
              playEvent Game.ApplyKing pSpot
            Just (Command.Killall pSpot) ->
              withBoard $ Board.setPart board pSpot (part {Board.inPlace = mempty})
              where
                part = Board.toPart board pSpot
            Just (Command.Reboot pSpot team) ->
              withBoard $ Board.setPart board pSpot part
              where
                (inHand, stack) =
                  Shared.getInitialDeck shared team
                    & map Card.cardToIdentifier
                    & splitAt Constants.initialHandSize
                part = (Board.empty team) {Board.inHand, Board.stack}
  where
    withBoard board' = noEff $ GameModel' $ gm {board = board'}
    playEvent eventMaker pSpot =
      updateModel
        (GameAction' $ Move.Sched $ Move.Play $ eventMaker pSpot)
        (GameModel' gm)
-- Actions that leave 'SinglePlayerView'
updateModel
  SinglePlayerBack
  (SinglePlayerLobbyModel' SinglePlayerLobbyModel {..}) =
    noEff $ WelcomeModel' $ initialWelcomeModel singlePlayerLobbyShared
updateModel
  SinglePlayerGo
  ( SinglePlayerLobbyModel'
      SinglePlayerLobbyModel
        { singlePlayerLobbyTeam = Just team,
          singlePlayerLobbyShared = shared
        }
    ) =
    noEff $ GameModel' $ level0GameModel Constants.Hard shared (Campaign.mkJourney team) (Board.Teams Undead team)
-- Actions that leave 'WelcomeView'
updateModel
  (WelcomeGo SinglePlayerDestination)
  (WelcomeModel' WelcomeModel {shared}) =
    noEff $ SinglePlayerLobbyModel' $ SinglePlayerLobbyModel Nothing shared
updateModel (WelcomeGo MultiPlayerDestination) (WelcomeModel' _) =
  effectSub (MultiPlayerLobbyModel' (CollectingUserName "")) $
    websocketSub (URL "ws://127.0.0.1:9160") (Protocols []) handleWebSocket
  where
    handleWebSocket (WebSocketMessage action) = MultiPlayerLobbyAction' (LobbyServerMessage action)
    handleWebSocket problem = traceShow problem NoOp
-- Actions that do not change the page delegate to more specialized versions
updateModel (SceneAction' action) (WelcomeModel' wm@WelcomeModel {sceneModel}) = do
  newSceneModel <- updateSceneModel action sceneModel
  return (WelcomeModel' wm {sceneModel = newSceneModel})
updateModel (SceneAction' _) model = noEff model
updateModel (Keyboard newKeysDown) (WelcomeModel' wm@WelcomeModel {keysDown, sceneModel}) = do
  newSceneModel <- maybe (return sceneModel) (`updateSceneModel` sceneModel) sceneAction
  return $ WelcomeModel' wm {keysDown = newKeysDown, sceneModel = newSceneModel}
  where
    sceneAction :: Maybe SceneAction
    sceneAction = asum (map keyCodeToSceneAction (toList (Set.difference newKeysDown keysDown)))
    keyCodeToSceneAction :: Int -> Maybe SceneAction
    keyCodeToSceneAction 80 = Just PauseOrResumeSceneForDebugging -- P key
    keyCodeToSceneAction _ = Nothing
updateModel (Keyboard _) model = noEff model
updateModel (GameAction' a) (GameModel' m@Model.Game {interaction}) =
  case runExcept (updateGameModel (m {anim = Game.NoAnimation}) a interaction) of
    Right (m, Nothing) -> noEff (GameModel' m)
    Right (m, nextSched) -> delayActions (GameModel' m) (nextSchedToMiso nextSched)
    Left errMsg -> noEff (GameModel' (m {interaction = ShowErrorInteraction errMsg}))
updateModel (LootAction' a) (LootModel' m) =
  noEff $ LootModel' $ updateLootModel a m
updateModel (SinglePlayerLobbyAction' a) (SinglePlayerLobbyModel' m) =
  noEff $ SinglePlayerLobbyModel' $ updateSinglePlayerLobbyModel a m
updateModel (MultiPlayerLobbyAction' a) (MultiPlayerLobbyModel' m) =
  MultiPlayerLobbyModel' `fmap` updateMultiPlayerLobbyModel a m
updateModel a m =
  error $
    LazyText.unpack $
      "Unhandled case in updateModel with the model being:\n"
        <> pShowNoColor m
        <> "\nand the action being:\n"
        <> pShowNoColor a

-- | The initial model, appropriately shuffled with 'Shared.Model' rng
level0GameModel ::
  Constants.Difficulty ->
  Shared.Model ->
  Campaign.Journey ->
  Board.Teams Team ->
  Model.Game
level0GameModel difficulty shared journey teams =
  levelNGameModel
    difficulty
    shared
    Campaign.Level0
    journey
    $ (teams <&> (\t -> (t, Shared.getInitialDeck shared t)))

-- | A model that takes the decks as parameters, for use after the initial
-- start of the game
levelNGameModel ::
  Constants.Difficulty ->
  Shared.Model ->
  Campaign.Level ->
  Campaign.Journey ->
  -- | The decks
  (Board.Teams (Team, [Card 'Core])) ->
  Model.Game
levelNGameModel difficulty shared level journey@(Campaign.Journey j) teams =
  assert correctOpponent Model.Game {..}
  where
    (_, board) = Board.initial shared teams
    interaction = NoInteraction
    playingPlayer = Spots.startingPlayerSpot
    playingPlayerDeck =
      Board.toData playingPlayer teams
        & snd
        & map Card.cardToIdentifier
    turn = Turn.initial
    anims = mempty
    anim = Game.NoAnimation
    uiAvail = True
    correctOpponent =
      (Just (fst $ Board.toData (Spots.other playingPlayer) teams))
        == (Map.lookup level j <&> fst)

-- | An initial model, with a custom board. /!\ Not for production /!\
unsafeInitialGameModel ::
  Constants.Difficulty ->
  Shared.Model ->
  -- | The initial decks
  (Board.Teams (Team, [Card 'Core])) ->
  -- | The board
  Board.T 'Core ->
  Model.Game
unsafeInitialGameModel difficulty shared teams board =
  Model.Game {..}
  where
    interaction = NoInteraction
    journey =
      Campaign.unsafeJourney
        level
        (Board.toData (Spots.other Spots.startingPlayerSpot) teams & fst)
    level = Campaign.Level0
    playingPlayer = Spots.startingPlayerSpot
    playingPlayerDeck =
      Board.toData playingPlayer teams
        & snd
        & map Card.cardToIdentifier
    turn = Turn.initial
    anims = mempty
    anim = Game.NoAnimation
    uiAvail = True

initialWelcomeModel :: Shared.Model -> WelcomeModel
initialWelcomeModel shared =
  WelcomeModel
    { sceneModel = toSceneModel Movie.welcomeMovie,
      keysDown = mempty,
      ..
    }
