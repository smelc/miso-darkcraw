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
module AI (AI.play, boardPlayerScore, placeCards) where

import Board
  ( Board,
    CardSpot,
    HandIndex (HandIndex),
    PlayerSpot,
    inTheBack,
    otherPlayerSpot,
  )
import qualified Board
import BoardInstances ()
import Card
import Control.Exception
import Control.Lens hiding (snoc)
import Data.Either.Extra
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as Text
import Debug.Trace (trace, traceShow)
import Game hiding (Event, Result)
import qualified Game
import Nat
import SharedModel (SharedModel)
import qualified SharedModel
import qualified Total

-- | Events that place creatures on the board. This function guarantees
-- that the returned events are solely placements (no neutral cards), so
-- that playing them with 'Game.playAll' returns an empty list of 'Game.Event'
placeCards ::
  SharedModel ->
  Board 'Core ->
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
  Board 'Core ->
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
      Board.toHand board pSpot
        & map (unliftCard . SharedModel.unsafeIdentToCard shared)
        & sortOn scoreHandCard
        & map cardToIdentifier
        & take 5 -- To keep complexity low for big hands
        & permutations -- Try cards in all orders (because aiPlayHand plays first card)
    possibles =
      map
        (\hand -> playHand shared (Board.setHand board pSpot hand) pSpot)
        hands
    scores :: [(Int, [Game.Event])] =
      map
        ( \events ->
            case Game.playAll shared board events of
              Left msg -> trace ("Maybe unexpected? " ++ Text.unpack msg) Nothing
              Right (Game.Result _ board' () _) -> Just (boardPlayerScore board' pSpot, events)
        )
        possibles
        & catMaybes
        & sortByFst

-- | The score of given player's in-place cards. Smaller is the best.
-- Both negative and positive values are returned.
boardPlayerScore :: Board 'Core -> PlayerSpot -> Int
boardPlayerScore board pSpot =
  sum scores
  where
    cSpotAndMayCreatures = Board.toPlayerHoleyInPlace board pSpot
    scores =
      map
        (\(cSpot, mayC) -> maybe 5 (\c -> scorePlace board c pSpot cSpot) mayC) -- Empty spot: malus of 5
        cSpotAndMayCreatures

-- | Events for playing all cards of the hand, in order. Each card
-- is played optimally.
playHand ::
  SharedModel ->
  Board 'Core ->
  -- | The playing player
  PlayerSpot ->
  [Game.Event]
playHand shared board pSpot =
  case aiPlayFirst shared board pSpot of
    Nothing -> []
    Just event ->
      assert (eventPlayerIs pSpot event) $
        case Game.playAll shared board [event] of
          Right (Game.Result shared' b' () _) ->
            event : playHand shared' b' pSpot
          Left msg ->
            traceShow ("Cannot play first card of hand: " ++ Text.unpack msg ++ ". Skipping it.") $
              playHand shared board' pSpot
            where
              -- The call to tail is safe because the hand must be non-empty,
              -- by aiPlayFirst returning Just _
              hand' = Board.toHand board pSpot & tail
              board' = Board.setHand board pSpot hand' -- Skip first card
  where
    eventPlayerIs expected =
      \case
        Place actual _ _ | expected == actual -> True
        Place' actual _ _ | expected == actual -> True
        Place {} -> False
        Place' {} -> False
        ApplyFearNTerror _ -> True -- we don't care
        Attack _ _ _ _ -> True -- no player, we're fine
        NoPlayEvent -> True -- no player, we're fine

-- | Take the hand's first card (if any) and return a [Place] event
-- for best placing this card.
aiPlayFirst ::
  SharedModel ->
  Board 'Core ->
  -- | The playing player, i.e. the player whose hand should the
  -- the card be picked from.
  PlayerSpot ->
  Maybe Game.Event
aiPlayFirst shared board pSpot =
  case Board.toHand board pSpot of
    [] -> Nothing
    id : _ -> do
      let scores' = scores id & sortByFst
      (_, target) <- listToMaybe scores'
      return $ Place' pSpot target id
  where
    handIndex = HandIndex 0
    scores :: ID -> [(Int, Target)] = \id ->
      [ (board' & eitherToMaybe <&> (\(Game.Result _ b _ _) -> boardPlayerScore b pSpot), target)
        | target <- targets board pSpot id,
          let board' = Game.play shared board $ Place pSpot target handIndex
      ]
        & map liftMaybe
        & catMaybes
    liftMaybe (Nothing, _) = Nothing
    liftMaybe (Just x, y) = Just (x, y)

targets ::
  Board 'Core ->
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
      Board.toPlayerCardSpots board pSpot ctk & map (CardTarget pSpot)

-- | The score of the card at the given position
scorePlace ::
  Board 'Core ->
  -- | The creature at 'pSpot' 'cSpot'
  Creature 'Core ->
  -- | Where to place the creature
  PlayerSpot ->
  -- | Where to place the creature
  CardSpot ->
  -- | The score of the card at pSpot cSpot. Smaller is the best.
  -- Both negative and positive values are returned.
  Int
scorePlace board inPlace pSpot cSpot =
  assert (creature ~=~ inPlace) result
  where
    (~=~) Nothing _ = False
    (~=~) (Just a) b = a == b
    creature :: Maybe (Creature 'Core) = Board.toInPlaceCreature board pSpot cSpot
    enemiesInPlace :: Map.Map CardSpot (Creature 'Core) =
      Board.toInPlace board (otherPlayerSpot pSpot)
    cSkills = skills inPlace
    prefersBack = Ranged' `elem` cSkills || LongReach' `elem` cSkills
    lineMalus = if inTheBack cSpot == prefersBack then 0 else 1
    enemySpots' :: [CardSpot] = allEnemySpots cSpot
    enemiesInColumn = map (enemiesInPlace Map.!?) enemySpots'
    -- TODO @smelc instead, play EndTurn pSpot cSpot and look at the score
    -- increase. This will avoid doing an incorrect simulation.
    yieldsVictoryPoints = all isNothing enemiesInColumn
    victoryPointsMalus = if yieldsVictoryPoints then 0 else 1
    maluses = sum [lineMalus, victoryPointsMalus]
    result = (assert (maluses >= 0) maluses) - scoreCreatureItems inPlace cSpot

-- | The score of the items of this creature (which is on the passed spot).
-- 0 is the worst. Higher values
-- are better. Only positive values are returned.
scoreCreatureItems :: Creature 'Core -> CardSpot -> Int
scoreCreatureItems c@Creature {attack, hp, items} cSpot =
  assert (result >= 0) result
  where
    scoreCreatureItem :: Item -> Int = \case
      Crown ->
        levelUpBonus + positionBonus
        where
          levelUpBonus = if Total.isDisciplined c then 0 else 2
          positionBonus =
            if (Board.neighbors Board.Cardinal cSpot & length) == 3
              then 1 -- Creature is in a central spot
              else 0
      FlailOfTheDamned -> natToInt $ attack + hp -- Prefer strong creatures
      SwordOfMight -> natToInt $ attack + hp -- Prefer strong creatures
    result = sum $ map scoreCreatureItem items

-- | The score of a card. Most powerful cards return a smaller value.
-- Negative values returned. FIXME @smelc Return positive values
-- Used to play most powerful cards first.
scoreHandCard :: Card 'Core -> Int
scoreHandCard = \case
  CreatureCard Creature {..} ->
    sum $ [- (natToInt hp), - (natToInt attack), - (fromMaybe 0 moral)] ++ map scoreSkill skills
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
