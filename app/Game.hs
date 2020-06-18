{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game
  ( PlayAction (..),
    play,
  )
where

import Board
import Card
import Control.Lens
import Data.Generics.Labels
import Data.List (delete)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Text as Text

-- * This module contains the game mechanic i.e. the function
-- that takes a 'Board', a 'PlayAction', and returns an updated 'Board'

-- TODO smelc Use a type family to share this type with Update.PlayAction
data PlayAction
  = -- | Player finishes its turn, we should resolve it
    EndPlayerTurn
  | -- | Player puts a card from his hand on its part of the board
    Place (Card Core) CardSpot

play :: Board -> PlayAction -> Either Text.Text Board
play board EndPlayerTurn = Right $ endPlayerTurn board
play board (Place (card :: Card Core) cSpot)
  | length hand == length hand' -- number of cards in hand did not decrease,
  -- this means the card wasn't in the hand to begin with
    =
    Left
      $ Text.pack
      $ "Trying to place card not in hand: " <> show card
  | Map.size onTable == Map.size onTable' -- number of cards on table
  -- did not grow, this means the spot wasn't empty
    =
    Left
      $ Text.pack
      $ "Cannot place card on non-empty spot: " <> show cSpot
  | otherwise = Right $ board {playerBottom = playerPart'}
  where
    hand :: [Card Core] = boardToHand board playingPlayerPart
    hand' :: [Card Core] = delete card hand
    onTable :: Map.Map CardSpot (Creature Core) =
      board ^. playingPlayerPart . #inPlace
    onTable' = onTable & (at cSpot ?~ cardToCreature card)
    playerPart' = PlayerPart {inPlace = onTable', inHand = hand'}

endPlayerTurn :: Board -> Board
endPlayerTurn = undefined
