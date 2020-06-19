{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Maybe (isJust, isNothing)
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
play board EndPlayerTurn = Right $ endTurn board playingPlayerSpot
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

endTurn :: Board -> PlayerSpot -> Board
endTurn = undefined

attack :: Board -> PlayerSpot -> CardSpot -> Board
attack board pSpot cSpot
  | isNothing attacker = board -- nothing to do
  | isJust allyBlocker = board -- cannot attack
  | otherwise = undefined
  where
    pSpotLens = spotToLens pSpot
    pOtherSpotLens = spotToLens $ otherPlayerSpot pSpot
    attackerInPlace :: Map.Map CardSpot (Creature Core) =
      board ^. (pSpotLens . #inPlace)
    attacker :: Maybe (Creature Core) = attackerInPlace Map.!? cSpot
    skills' :: [Skill] = attacker >>= skills & reduce
    reduce (Just a) = a
    reduce Nothing = []
    allyBlockerSpot' :: Maybe CardSpot =
      if Ranged `elem` skills' || HitFromBack `elem` skills'
        then Nothing -- attacker bypasses ally blocker (if any)
        else allyBlockerSpot cSpot
    allyBlocker :: Maybe (Creature Core) =
      allyBlockerSpot' >>= (attackerInPlace Map.!?)
    attackedSpots' :: [CardSpot] = attackedSpots cSpot
    attacked :: Map.Map CardSpot (Creature Core) =
      board ^. (pOtherSpotLens . #inPlace)
    attacked' :: [(CardSpot, Creature Core)] =
      Map.toList attacked & filter (\(c, _) -> c `elem` attackedSpots')

-- | The spot that blocks a spot from attacking, which happens
-- | if the input spot is in the back line
allyBlockerSpot :: CardSpot -> Maybe CardSpot
allyBlockerSpot TopLeft = Just BottomLeft
allyBlockerSpot Top = Just Bottom
allyBlockerSpot TopRight = Just BottomRight
allyBlockerSpot _ = Nothing

inTheBack :: CardSpot -> Bool
inTheBack TopLeft = True
inTheBack Top = True
inTheBack TopRight = True
inTheBack _ = False

otherSpotInColumn :: CardSpot -> CardSpot
otherSpotInColumn TopLeft = BottomLeft
otherSpotInColumn Top = Bottom
otherSpotInColumn TopRight = BottomRight
otherSpotInColumn BottomLeft = TopLeft
otherSpotInColumn Bottom = Top
otherSpotInColumn BottomRight = TopRight

-- | Spots that can be attacked from a spot. Spot as argument is
-- | a player part while spots returned are in the opposing player part.
-- | The order in the result matters, the first element is the first spot
-- | attacked, then the second element is attacked is the first element
-- | gets killed, etc.
attackedSpots :: CardSpot -> [CardSpot]
attackedSpots cSpot = [otherSpotInColumn cSpot, cSpot]
