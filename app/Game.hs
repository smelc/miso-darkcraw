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
  = -- | Player puts a card from his hand on its part of the board
    Place (Card Core) CardSpot

play :: Board -> PlayAction -> Either Text.Text Board
play board (Place (card :: Card Core) cSpot)
  | length hand' == length hand =
    Left
      $ Text.pack
      $ "Trying to place card not in hand: " <> show card
  | isJust onSpot =
    Left
      $ Text.pack
      $ "Cannot place card on non-empty spot: " <> show cSpot
  | otherwise = Right $ board {playerBottom = playerPart'}
  where
    hand :: [Card Core] = boardToHand board playingPlayerPart
    hand' :: [Card Core] = delete card hand
    -- XXX alternatively update map with lens and check new map
    -- did grow?
    onTable :: Map.Map CardSpot (Creature Core) =
      board ^. playingPlayerPart . #inPlace
    onSpot :: Maybe (Creature Core) = Map.lookup cSpot onTable
    onTable' = Map.insert cSpot (cardToCreature card) onTable
    playerPart' = PlayerPart {inPlace = onTable', inHand = hand'}
