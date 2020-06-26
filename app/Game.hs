{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Game
  ( attackOrder, -- Exported only for tests
    enemySpots,
    PlayAction (..),
    play,
  )
where

import Board
import Card
import Control.Lens
import Control.Monad.Except
import Control.Monad.Writer
import Data.Bifunctor
import Data.Foldable
import Data.Generics.Labels
import Data.List (delete)
import Data.Map.Strict ((!?), Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

-- * This module contains the game mechanic i.e. the function
-- that takes a 'Board', a 'PlayAction', and returns an updated 'Board'

-- TODO smelc Use a type family to share this type with Update.PlayAction
data PlayAction
  = -- | Player finishes its turn, we should resolve it
    EndPlayerTurn
  | -- | Player puts a card from his hand on its part of the board
    Place (Card Core) CardSpot

reportEffect :: MonadWriter AttackEffects m => CardSpot -> AttackEffect -> m ()
reportEffect cSpot effect = tell $ AttackEffects (Map.singleton cSpot effect)

play :: Board Core -> PlayAction -> Either Text (Board Core, AttackEffects)
play board action =
  playM board action
    & runWriterT
    & runExcept

playM ::
  MonadError Text m =>
  MonadWriter AttackEffects m =>
  Board Core ->
  PlayAction ->
  m (Board Core)
playM board EndPlayerTurn = endTurn board playingPlayerSpot
playM board (Place (card :: Card Core) cSpot)
  | length hand == length hand' -- number of cards in hand did not decrease,
  -- this means the card wasn't in the hand to begin with
    =
    throwError $
      "Trying to place card not in hand: " <> Text.pack (show card)
  | Map.size onTable == Map.size onTable' -- number of cards on table
  -- did not grow, this means the spot wasn't empty
    =
    throwError $
      "Cannot place card on non-empty spot: " <> Text.pack (show cSpot)
  | otherwise = return $ board {playerBottom = playerPart'}
  where
    hand :: [Card Core] = boardToHand board playingPlayerPart
    hand' :: [Card Core] = delete card hand
    onTable :: Map CardSpot (Creature Core) =
      board ^. playingPlayerPart . #inPlace
    onTable' = onTable & at cSpot ?~ cardToCreature card
    playerPart' = PlayerPart {inPlace = onTable', inHand = hand'}

endTurn ::
  MonadWriter AttackEffects m =>
  -- | The input board
  Board Core ->
  -- | The player whose turn is ending
  PlayerSpot ->
  m (Board Core)
endTurn board pSpot =
  foldrM applyCard board attackOrder
  where
    applyCard cSpot board = Game.attack board pSpot cSpot

-- | Card at [pSpot],[cSpot] attacks; causing changes to a board
attack ::
  MonadWriter AttackEffects m =>
  Board Core ->
  PlayerSpot ->
  CardSpot ->
  m (Board Core)
attack board pSpot cSpot =
  case (attacker, allyBlocker, attacked) of
    (_, Just _, _) -> return board -- an ally blocks the way
    (Just hitter, _, Just (hitSpot, hittee)) ->
      -- attack can proceed
      let effect = singleAttack hitter hittee
          newHittee = applyAttackEffect effect hittee
       in do
            reportEffect hitSpot effect
            return (board & pOtherSpotLens . #inPlace . at hitSpot .~ newHittee)
    _ -> return board -- no attacker or nothing to attack
  where
    pSpotLens = spotToLens pSpot
    pOtherSpotLens :: Lens' (Board Core) (PlayerPart Core)
    pOtherSpotLens = spotToLens $ otherPlayerSpot pSpot
    attackerInPlace :: Map CardSpot (Creature Core) =
      board ^. pSpotLens . #inPlace
    attacker :: Maybe (Creature Core) = attackerInPlace !? cSpot
    attackerSkills :: [Skill] = attacker >>= skills & fromMaybe []
    allyBlocker :: Maybe (Creature Core) =
      if any (`elem` attackerSkills) [Ranged, HitFromBack]
        then Nothing -- attacker bypasses ally blocker (if any)
        else allyBlockerSpot cSpot >>= (attackerInPlace !?)
    attackedSpots :: [CardSpot] = enemySpots attackerSkills cSpot
    -- For the moment a card attacks the first card in front of it. If
    -- later there's a skill Rampage, this will change:
    attacked :: Maybe (CardSpot, Creature Core) =
      board ^@? pOtherSpotLens . #inPlace . ifolded . indices (`elem` attackedSpots)

applyAttackEffect :: AttackEffect -> Creature Core -> Maybe (Creature Core)
applyAttackEffect effect creature@Creature {..} =
  case effect of
    Death -> Nothing
    HitPointsChange i -> Just $ creature {hp = hp + i}

-- The effect of an attack on the defender
singleAttack :: Creature Core -> Creature Core -> AttackEffect
singleAttack attacker defender
  | hps' <= 0 = Death
  | otherwise = HitPointsChange $ - hit
  where
    hit = Card.attack attacker
    hps' = Card.hp defender - hit

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

-- | The other spot in the column in the spot's part
otherYSpot :: CardSpot -> CardSpot
otherYSpot TopLeft = BottomLeft
otherYSpot Top = Bottom
otherYSpot TopRight = BottomRight
otherYSpot BottomLeft = TopLeft
otherYSpot Bottom = Top
otherYSpot BottomRight = TopRight

-- | Spots that can be attacked from a spot
-- | Spot as argument is in one player part while spots returned
-- | are in the other player part.
-- | The order in the result matters, the first element is the first spot
-- | attacked, then the second element is attacked if the first spot is empty
enemySpots :: [Skill] -> CardSpot -> [CardSpot]
enemySpots skills cSpot =
  map fst result
  where
    enemyYSpot' = \case
      TopLeft -> zip [TopLeft, BottomLeft] [1 ..]
      Top -> zip [Top, Bottom] [1 ..]
      TopRight -> zip [TopRight, BottomRight] [1 ..]
      BottomLeft -> zip [TopLeft, BottomLeft] [0 ..]
      Bottom -> zip [Top, Bottom] [0 ..]
      BottomRight -> zip [TopRight, BottomRight] [0 ..]
    all :: [(CardSpot, Int)] = enemyYSpot' cSpot & map (Data.Bifunctor.first bottomSpotOfTopVisual)
    result
      | Ranged `elem` skills = all -- ranged attacker can attack anywhere in its column
      | HitFromBack `elem` skills && inTheBack cSpot = take 1 all
      | otherwise =
        if null all || ((Prelude.head all & snd) > 0)
          then []
          else take 1 all

-- | The order in which cards attack
attackOrder :: [CardSpot]
attackOrder = [BottomRight, Bottom, BottomLeft, TopRight, Top, TopLeft]
