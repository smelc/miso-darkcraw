{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This module defines how the AI plays
-- |
module AI (aiPlay, placeCards) where

import Board
import Card
import Control.Exception
import Control.Lens hiding (snoc)
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as Text
import Debug.Trace (trace)
import Game
import Turn (Turn, turnToPlayerSpot)

placeCards ::
  Board Core ->
  Turn ->
  [GamePlayEvent]
placeCards board turn =
  -- Will fail once when we do more stuff in aiPlay. It's OK, I'll
  -- adapt when this happens.
  assert (all isPlaceEvent events) events
  where
    events = aiPlay board turn
    isPlaceEvent EndTurn {} = False
    isPlaceEvent NoPlayEvent = False
    isPlaceEvent Place {} = True

-- | Smart play events
aiPlay :: Board Core -> Turn -> [GamePlayEvent]
aiPlay board turn =
  case scores of
    [] -> []
    (_, events) : _ -> events
  where
    pSpot = turnToPlayerSpot turn
    hands :: [[Card Core]] = permutations $ boardToHand board pSpot
    possibles :: [[GamePlayEvent]] =
      map (\hand -> aiPlayHand (boardSetHand board pSpot hand) turn) hands
    scores :: [(Int, [GamePlayEvent])] =
      map
        ( \events ->
            case playAll board events of
              Left msg -> trace ("Unexpected case: " ++ Text.unpack msg) Nothing
              Right board' -> Just (boardScore board' turn, events)
        )
        possibles
        & catMaybes
        & sortByFst
    playAll b events = Game.playAll b events <&> fst

-- | The score of the given player's in-place cards. 0 is the best.
boardScore :: Board Core -> Turn -> Int
boardScore board turn =
  sum scores
  where
    pSpot = turnToPlayerSpot turn
    cSpotAndMayCreatures =
      boardToHoleyInPlace board & filter (\(pSpot', _, _) -> pSpot == pSpot')
        & map (\(_, snd, thd) -> (snd, thd))
    scores =
      map
        ( \(cSpot, mCreature) ->
            case mCreature of
              Nothing -> 1 -- Empty spot: malus
              Just creature -> scorePlace board (pSpot, cSpot) creature
        )
        cSpotAndMayCreatures

-- | Events for playing all cards of the hand, in order. Each card
-- is placed at an optimal position.
aiPlayHand :: Board Core -> Turn -> [GamePlayEvent]
aiPlayHand board turn =
  case aiPlayFirst board turn of
    Nothing -> []
    Just place ->
      case Game.play board place of
        Left msg -> error $ Text.unpack msg -- We shouldn't generate invalid Place actions
        Right (b', _) ->
          place : aiPlayHand b' turn

-- | Take the hand's first card (if any) and return a [Place] event
-- for best placing this card.
aiPlayFirst :: Board Core -> Turn -> Maybe GamePlayEvent
aiPlayFirst board turn = do
  card <- boardToHand board pSpot & listToMaybe
  creature <- cardToCreature card
  let scores = scoresf creature & sortByFst
  best <- listToMaybe scores
  return $ Place pSpot (snd best) $ HandIndex 0
  where
    pSpot = turnToPlayerSpot turn
    possibles = placements board pSpot
    scoresf creature =
      [(scorePlace board (pSpot, cSpot) creature, cSpot) | cSpot <- possibles]

placements ::
  Board Core ->
  -- | The player placing a card
  PlayerSpot ->
  -- | All spots where the card can be put
  [CardSpot]
placements board pSpot =
  [cSpot | cSpot <- allCardsSpots, cSpot `Map.notMember` inPlace]
  where
    pLens = spotToLens pSpot
    inPlace :: Map.Map CardSpot (Creature Core) = board ^. pLens . #inPlace

-- | The score of placing a card at the given position
scorePlace ::
  Board Core ->
  -- | Where to place the card
  (PlayerSpot, CardSpot) ->
  -- | The card to place
  Creature Core ->
  -- | The score of placing the card, 0 is the best.
  Int
scorePlace board (pSpot, cSpot) card =
  case inPlace of
    Just _ -> 999 -- Target spot is occupied
    _ -> sum maluses
  where
    pLens = spotToLens pSpot
    otherPLens = spotToLens $ otherPlayerSpot pSpot
    inPlace :: Maybe (Creature Core) = board ^. pLens . #inPlace . at cSpot
    enemiesInPlace :: Map.Map CardSpot (Creature Core) =
      board ^. otherPLens . #inPlace
    cSkills :: [Skill] = skills card & fromMaybe []
    prefersBack = Ranged `elem` cSkills || LongReach `elem` cSkills
    backMalus :: Int = if prefersBack && not (inTheBack cSpot) then 1 else 0
    enemySpots' :: [CardSpot] = allEnemySpots cSpot
    enemiesInColumn = map (enemiesInPlace Map.!?) enemySpots'
    yieldsVictoryPoints = all isNothing enemiesInColumn
    victoryPointsMalus = if yieldsVictoryPoints then 0 else 1
    maluses = [backMalus, victoryPointsMalus]

sortByFst l =
  sortBy sortFst l
  where
    sortFst (i, _) (j, _) = compare i j
