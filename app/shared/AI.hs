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
import Data.Either
import Data.Either.Extra
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as Text
import Debug.Trace (trace)
import Game
import SharedModel (SharedModel)
import Turn (Turn, turnToPlayerSpot)

placeCards ::
  SharedModel ->
  Board Core ->
  Turn ->
  [GamePlayEvent]
placeCards shared board turn =
  -- Will fail once when we do more stuff in aiPlay. It's OK, I'll
  -- adapt when this happens.
  assert (all isPlaceEvent events) events
  where
    events = aiPlay shared board turn
    isPlaceEvent EndTurn {} = False
    isPlaceEvent NoPlayEvent = False
    isPlaceEvent Place {} = True
    isPlaceEvent Place' {} = True

-- | Smart play events
aiPlay :: SharedModel -> Board Core -> Turn -> [GamePlayEvent]
aiPlay shared board turn =
  case scores of
    [] -> []
    (_, events) : _ -> events
  where
    pSpot = turnToPlayerSpot turn
    hands = boardToHand board pSpot & permutations
    possibles =
      map
        (\hand -> aiPlayHand shared (boardSetHand board pSpot hand) turn)
        hands
    scores :: [(Int, [GamePlayEvent])] =
      map
        ( \events ->
            case Game.playAll shared board events <&> fst of
              Left msg -> trace ("Unexpected case: " ++ Text.unpack msg) Nothing
              Right board' -> Just (boardScore board' turn, events)
        )
        possibles
        & catMaybes
        & sortByFst

-- | The score of the given player's in-place cards. 0 is the best.
boardScore :: Board Core -> Turn -> Int
boardScore board turn =
  sum scores
  where
    pSpot = turnToPlayerSpot turn
    cSpotAndMayCreatures =
      boardToHoleyInPlace board & filter (\(pSpot', _, _) -> pSpot == pSpot')
        & map (\(_, cSpot, mCreature) -> (cSpot, mCreature))
    scores =
      map
        ( \(cSpot, mCreature) ->
            case mCreature of
              Nothing -> 5 -- Empty spot: malus
              Just _ ->
                let score = scorePlace board pSpot cSpot
                 in assert (isJust score) (fromJust score)
        )
        cSpotAndMayCreatures

-- | Events for playing all cards of the hand, in order. Each card
-- is placed at an optimal position.
aiPlayHand :: SharedModel -> Board Core -> Turn -> [GamePlayEvent]
aiPlayHand shared board turn =
  case aiPlayFirst shared board turn of
    Nothing -> []
    Just place ->
      case Game.play shared board place of
        Left msg -> error $ Text.unpack msg -- We shouldn't generate invalid Place actions
        Right (b', _) ->
          place : aiPlayHand shared b' turn

-- | Take the hand's first card (if any) and return a [Place] event
-- for best placing this card.
aiPlayFirst :: SharedModel -> Board Core -> Turn -> Maybe GamePlayEvent
aiPlayFirst shared board turn =
  case boardToHand board pSpot of
    [] -> Nothing
    IDC creatureID : _ -> do
      let scores' = scores & sortByFst
      best <- listToMaybe scores'
      return $ Place' pSpot (snd best) creatureID
    i : _ -> error $ "Unsupported identifier: " ++ show i
  where
    handIndex = HandIndex 0
    pSpot = turnToPlayerSpot turn
    possibles = placements board pSpot
    scores =
      [ (scorePlace (fromRight' board' & fst) pSpot cSpot, cSpot)
        | cSpot <- possibles,
          let place = Place pSpot cSpot handIndex,
          let board' = Game.play shared board place,
          isRight board'
      ]

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
  PlayerSpot ->
  CardSpot ->
  -- | The score of the card at pSpot cSpot. 0 is the best.
  Maybe Int
scorePlace board pSpot cSpot =
  case inPlace of
    Just _ -> Just $ sum maluses
    _ -> Nothing
  where
    inPlace :: Maybe (Creature Core) = boardToInPlaceCreature board pSpot cSpot
    enemiesInPlace :: Map.Map CardSpot (Creature Core) =
      boardToInPlace board (otherPlayerSpot pSpot)
    cSkills = inPlace <&> skills & fromMaybe []
    prefersBack = Ranged `elem` cSkills || LongReach `elem` cSkills
    lineMalus = if inTheBack cSpot == prefersBack then 0 else 1
    enemySpots' :: [CardSpot] = allEnemySpots cSpot
    enemiesInColumn = map (enemiesInPlace Map.!?) enemySpots'
    -- TODO @smelc instead, play EndTurn pSpot cSpot and look at the score
    -- increase. This will avoid doing an incorrect simulation.
    yieldsVictoryPoints = all isNothing enemiesInColumn
    victoryPointsMalus = if yieldsVictoryPoints then 0 else 1
    maluses = [lineMalus, victoryPointsMalus]

sortByFst l =
  sortBy sortFst l
  where
    sortFst (i, _) (j, _) = compare i j
