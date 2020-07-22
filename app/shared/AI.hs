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
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Game (PlayAction (..), allEnemySpots)
import Turn (Turn, turnToPlayerSpot)

-- | Crafts a play action for the player whose turn it is.
-- | Returns 'EndPlayerTurn' if the player cannot do anything.
aiPlay :: Board Core -> Turn -> PlayAction
aiPlay board turn = aiPlay' board $ turnToPlayerSpot turn

aiPlay' :: Board Core -> PlayerSpot -> PlayAction
aiPlay' board pSpot =
  undefined

-- | The score of placing a card at the given position
scorePlace ::
  Board Core ->
  -- | Where to place the card
  (PlayerSpot, CardSpot) ->
  -- | The card to place
  Creature Core ->
  -- | The score of placing the card, 0 is the best. 'Nothing' if the card
  -- | cannot be placed
  Maybe Int
scorePlace board (pSpot, cSpot) card =
  case inPlace of
    Just _ -> Nothing -- Target spot is occupied
    _ -> Just backMalus
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
