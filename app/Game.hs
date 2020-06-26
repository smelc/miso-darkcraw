{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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
import Data.Bifunctor
import Data.Generics.Labels
import Data.List (delete)
import Data.Map.Strict ((!?), Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing, listToMaybe)
import qualified Data.Text as Text

-- * This module contains the game mechanic i.e. the function
-- that takes a 'Board', a 'PlayAction', and returns an updated 'Board'

-- TODO smelc Use a type family to share this type with Update.PlayAction
data PlayAction
  = -- | Player finishes its turn, we should resolve it
    EndPlayerTurn
  | -- | Player puts a card from his hand on its part of the board
    Place (Card Core) CardSpot

data AttackEffect
  = -- | Death (hits points change that makes hit points go 0 or negative)
    Death
  | -- | Hit points change
    HitPointsChange Int

-- TODO put Board UI as second member which will map CardSpot to AttackEffect
-- instead of mapping them to Creature (as Board Core does)
type PlayResult = (Board Core, Map.Map CardSpot AttackEffect)

play :: Board Core -> PlayAction -> Either Text.Text PlayResult
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
  | otherwise = Right (board {playerBottom = playerPart'}, Map.empty)
  where
    hand :: [Card Core] = boardToHand board playingPlayerPart
    hand' :: [Card Core] = delete card hand
    onTable :: Map CardSpot (Creature Core) =
      board ^. playingPlayerPart . #inPlace
    onTable' = onTable & (at cSpot ?~ cardToCreature card)
    playerPart' = PlayerPart {inPlace = onTable', inHand = hand'}

endTurn ::
  -- | The input board
  Board Core ->
  -- | The player whose turn is ending
  PlayerSpot ->
  PlayResult
endTurn board pSpot =
  Prelude.foldr f initial attackOrder
  where
    initial = (board, Map.empty)
    f :: CardSpot -> PlayResult -> PlayResult
    f cSpot (board', effects) = (board'', effectsUnion effects effects')
      where
        (board'', effects') = Game.attack board' pSpot cSpot
        effectsUnion = Map.unionWith reduceAttackEffect

-- | Card at [pSpot],[cSpot] attacks; causing changes to a board
attack :: Board Core -> PlayerSpot -> CardSpot -> PlayResult
attack board pSpot cSpot =
  case (attacker, allyBlocker, attacked) of
    (_, Just _, _) -> noChange -- an ally blocks the way
    (Just hitter, _, Just (hitSpot, hittee)) ->
      -- attack can proceed
      let effect = singleAttack hitter hittee
          newHittee = applyAttackEffect effect hittee
       in ( board & pOtherSpotLens . #inPlace . at hitSpot .~ newHittee,
            Map.singleton hitSpot effect
          )
    _ -> noChange -- no attacker or nothing to attack
  where
    noChange = (board, Map.empty)
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

reduceAttackEffect :: AttackEffect -> AttackEffect -> AttackEffect
reduceAttackEffect Death _ = Death
reduceAttackEffect _ Death = Death
reduceAttackEffect (HitPointsChange i) (HitPointsChange j) =
  HitPointsChange (i + j)

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
