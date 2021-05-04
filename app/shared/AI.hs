{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This module defines how the AI plays
-- |
-- boardScore is exported for tests
module AI (AI.play, boardScore, placeCards) where

import Board
import BoardInstances ()
import Card
import Control.Exception
import Control.Lens hiding (snoc)
import Data.Either
import Data.Either.Extra
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as Text
import Debug.Trace (trace, traceShow)
import Game hiding (Event, Result)
import qualified Game
import SharedModel (SharedModel)
import qualified SharedModel
import Turn (Turn)
import qualified Turn

-- | Events that place creatures on the board. This function guarantees
-- that the returned events are solely placements (no neutral cards), so
-- that playing them with 'Game.playAll' returns an empty list of 'Game.Event'
placeCards ::
  SharedModel ->
  Board Core ->
  -- | The player whose cards must be played
  PlayerSpot ->
  [Game.Event]
placeCards shared board turn =
  -- Will fail once when we do more stuff in aiPlay. It's OK, I'll
  -- adapt when this happens.
  assert (all isPlaceEvent events) events
  where
    events = AI.play shared board turn
    isPlaceEvent ApplyFearNTerror {} = False
    isPlaceEvent Attack {} = False
    isPlaceEvent NoPlayEvent = False
    isPlaceEvent Place {} = True
    isPlaceEvent Place' {} = True

-- | Smart play events
play ::
  SharedModel ->
  Board Core ->
  -- | The playing player
  PlayerSpot ->
  -- | Events generated for player 'pSpot'
  [Game.Event]
play shared board pSpot =
  case scores of
    [] -> []
    (_, events) : _ -> events
  where
    hands =
      boardToHand board pSpot
        & map (unliftCard . SharedModel.unsafeIdentToCard shared)
        & sortOn scoreCard
        & map cardToIdentifier
        & take 5 -- To keep complexity low for big hands
        & permutations -- Try cards in all orders (because aiPlayHand plays first card)
    possibles =
      map
        (\hand -> playHand shared (boardSetHand board pSpot hand) pSpot)
        hands
    scores :: [(Int, [Game.Event])] =
      map
        ( \events ->
            case Game.playAll shared board events of
              Left msg -> trace ("Maybe unexpected? " ++ Text.unpack msg) Nothing
              Right (Game.Result _ board' () _) -> Just (boardScore board' pSpot, events)
        )
        possibles
        & catMaybes
        & sortByFst

-- | The score of a board, from the POV of the given player. Smaller is
-- the best
boardScore :: Board Core -> PlayerSpot -> Int
boardScore board pSpot =
  -- This is wrong (breaks testAIRanged), I don't get why.
  -- playerScore (otherPlayerSpot pSpot) - playerScore pSpot
  -- where
  --   playerScore pSpot =
  --     boardPlayerScore board pSpot + (boardToPart board pSpot & score)
  boardPlayerScore board pSpot

-- | The score of given player's in-place cards. 0 is the best.
boardPlayerScore :: Board Core -> PlayerSpot -> Int
boardPlayerScore board pSpot =
  sum scores
  where
    cSpotAndMayCreatures = boardToPlayerHoleyInPlace board pSpot
    scores =
      map
        (\(cSpot, mayC) -> maybe 5 (\c -> scorePlace board c pSpot cSpot) mayC) -- Empty spot: malus of 5
        cSpotAndMayCreatures

-- | Events for playing all cards of the hand, in order. Each card
-- is played optimally.
playHand ::
  SharedModel ->
  Board Core ->
  -- | The playing player
  PlayerSpot ->
  [Game.Event]
playHand shared board pSpot =
  case aiPlayFirst shared board pSpot of
    Nothing -> []
    Just event ->
      case Game.playAll shared board [event] of
        Left msg ->
          traceShow ("Cannot play first card of hand: " ++ Text.unpack msg) $
            playHand shared board' pSpot
          where
            hand' = boardToHand board pSpot & tail
            board' = boardSetHand board pSpot hand' -- Skip first card
        Right (Game.Result shared' b' () _) ->
          event : playHand shared' b' pSpot

-- | Take the hand's first card (if any) and return a [Place] event
-- for best placing this card.
aiPlayFirst ::
  SharedModel ->
  Board Core ->
  -- | The playing player, i.e. the player whose hand should the
  -- the card be picked from.
  PlayerSpot ->
  Maybe Game.Event
aiPlayFirst shared board pSpot =
  case boardToHand board pSpot of
    [] -> Nothing
    id : _ -> do
      let scores' = scores id & sortByFst
      best <- listToMaybe scores'
      return $ Place' (snd best) id
  where
    handIndex = HandIndex 0
    possibles id = targets board pSpot id
    scores id =
      [ (boardScore (fromRight' board' & takeBoard) pSpot, target)
        | target <- possibles id,
          let board' = Game.play shared board $ Place target handIndex,
          isRight board'
      ]
    takeBoard (Game.Result _ b _ _) = b

targets ::
  Board Core ->
  -- | The player placing a card
  PlayerSpot ->
  -- | The card being played
  Card.ID ->
  -- | All spots where the card can be put
  [Target]
targets board playingPlayer id =
  case (Card.targetType id, whichPlayerTarget id) of
    (CardTargetType ctk, Game.Playing) ->
      cardTargets playingPlayer ctk
    (CardTargetType ctk, Opponent) ->
      cardTargets (otherPlayerSpot playingPlayer) ctk
    (PlayerTargetType, Playing) ->
      [PlayerTarget playingPlayer]
    (PlayerTargetType, Opponent) ->
      [PlayerTarget $ otherPlayerSpot playingPlayer]
  where
    cardTargets pSpot ctk =
      boardToPlayerCardSpots board pSpot ctk & map (CardTarget pSpot)

-- | The score of the card at the given position
scorePlace ::
  Board Core ->
  -- | The creature at 'pSpot' 'cSpot'
  Creature 'Core ->
  -- | Where to place the creature
  PlayerSpot ->
  -- | Where to place the creature
  CardSpot ->
  -- | The score of the card at pSpot cSpot. 0 is the best.
  Int
scorePlace board inPlace pSpot cSpot =
  assert (creature ~=~ inPlace) $ sum maluses
  where
    (~=~) Nothing _ = False
    (~=~) (Just a) b = a == b
    creature :: Maybe (Creature Core) = boardToInPlaceCreature board pSpot cSpot
    enemiesInPlace :: Map.Map CardSpot (Creature Core) =
      boardToInPlace board (otherPlayerSpot pSpot)
    cSkills = skills inPlace
    prefersBack = Ranged' `elem` cSkills || LongReach' `elem` cSkills
    lineMalus = if inTheBack cSpot == prefersBack then 0 else 1
    enemySpots' :: [CardSpot] = allEnemySpots cSpot
    enemiesInColumn = map (enemiesInPlace Map.!?) enemySpots'
    -- TODO @smelc instead, play EndTurn pSpot cSpot and look at the score
    -- increase. This will avoid doing an incorrect simulation.
    yieldsVictoryPoints = all isNothing enemiesInColumn
    victoryPointsMalus = if yieldsVictoryPoints then 0 else 1
    maluses = [lineMalus, victoryPointsMalus]

-- | The score of a card. Most powerful cards return a smaller value.
-- Negative values returned. FIXME @smelc Return positive values
scoreCard :: Card Core -> Int
scoreCard = \case
  CreatureCard Creature {..} ->
    sum $ [- hp, - attack, - (fromMaybe 0 moral)] ++ map scoreSkill skills
  NeutralCard NeutralObject {neutral} ->
    case neutral of
      Health -> -1
      InfernalHaste -> -10
      Life -> -3
      Plague -> -5 -- TODO @smelc pass the board and check number of creates, for a dynamic value to be returned
  ItemCard ItemObject {item} ->
    case item of
      Crown -> -1
      FlailOfTheDamned -> -1
      SwordOfMight -> -1

-- | The score of a skill, smaller values are better. Negative values returned.
scoreSkill :: SkillCore -> Int
scoreSkill s =
  case s of
    Blow' b -> if b then -1 else 0
    Discipline' -> -1
    DrawCard' b -> if b then -1 else 0
    Fear' b -> if b then -1 else 0
    LongReach' -> -1
    Ranged' -> -1
    Stupid4' _ -> if isStupid s then 2 else 1
    Terror' b -> if b then -2 else 0
    Unique' -> 0

sortByFst :: [(Int, b)] -> [(Int, b)]
sortByFst l =
  sortBy sortFst l
  where
    sortFst (i, _) (j, _) = compare i j
