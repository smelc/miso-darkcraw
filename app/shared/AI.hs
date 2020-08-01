{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This module defines how the AI plays
-- |
module AI (aiPlay) where

import Board
import Card
import Control.Lens
import Data.Generics.Labels
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Game (GamePlayEvent (..), allEnemySpots, play)
import Turn (Turn, turnToPlayerSpot)

-- | Crafts play actions for the player whose turn it is.
-- | The returned list is guaranteed not to contain 'EndPlayerTurn'. This
-- | function returns 'Either', because 'play' can.
aiPlay :: Board Core -> Turn -> Either Text [GamePlayEvent]
aiPlay board turn =
  helper board []
  where
    helper board' actions =
      case aiPlay' board' turn of
        EndTurn _ -> Right $ reverse actions
        action -> do
          (board'', _) <- play board' action
          helper board'' $ action : actions

-- | Crafts a play action for the player whose turn it is.
-- | Returns 'EndPlayerTurn' if the player cannot do anything.
aiPlay' :: Board Core -> Turn -> GamePlayEvent
aiPlay' board turn
  | null hand || null candidates = EndTurn pSpot -- hand is empty
  | otherwise =
    let (creature, cSpot, _) = head candidates
     in Place pSpot cSpot undefined
  where
    pSpot = turnToPlayerSpot turn
    hand :: [Card Core] = boardToHand board $ spotToLens pSpot
    hand' :: [Creature Core] = map cardToCreature hand & catMaybes
    places :: [CardSpot] = placements board pSpot
    -- It's not a min-max yet because we do not try to play the first
    -- card and see whether it helps putting a good second one, etc.
    scores :: [(Creature Core, CardSpot, Int)]
    scores =
      [ ( creature,
          cSpot,
          fromJust score
        )
        | creature <- hand',
          cSpot <- places,
          let score = scorePlace board (pSpot, cSpot) creature,
          isJust score
      ]
    sortThd (_, _, t1) (_, _, t2) = compare t1 t2
    candidates = sortBy sortThd scores

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
  -- | The score of placing the card, 0 is the best. 'Nothing' if the card
  -- | cannot be placed or if score cannot be beaten.
  Maybe Int
scorePlace board (pSpot, cSpot) card =
  case inPlace of
    Just _ -> Nothing -- Target spot is occupied
    _ -> Just $ sum maluses -- Can be optimized, by interrupting computations
    -- when partial sum is above the current best score
  where
    pLens = spotToLens pSpot
    otherPLens = spotToLens $ otherPlayerSpot pSpot
    inPlace :: Maybe (Creature Core) = board ^. pLens . #inPlace . at cSpot
    enemiesInPlace :: Map.Map CardSpot (Creature Core) =
      board ^. otherPLens . #inPlace
    cSkills :: [Skill] = skills card & fromMaybe []
    prefersBack = Ranged `elem` cSkills || HitFromBack `elem` cSkills
    backMalus :: Int = if prefersBack && not (inTheBack cSpot) then 1 else 0
    enemySpots' :: [CardSpot] = allEnemySpots cSpot
    enemiesInColumn = map (enemiesInPlace Map.!?) enemySpots'
    yieldsVictoryPoints = all isNothing enemiesInColumn
    victoryPointsMalus = if yieldsVictoryPoints then 0 else 1
    maluses = [backMalus, victoryPointsMalus]
