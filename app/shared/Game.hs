{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Game
  ( allEnemySpots,
    attackOrder, -- Exported only for tests
    enemySpots,
    GamePlayEvent (..),
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
-- that takes a 'Board', a 'GamePlayAct', and returns an updated 'Board'

-- | TODO smelc Use a type family to share this type with Update.PlayAction
-- | This type is used internally in this module. UI actions do not produce
-- | instances of this type, they produce instances of 'Update.PlayAct'
-- | which get translated into this type in 'Update'
data GamePlayEvent
  = -- | A player finishes its turn, we should resolve it
    EndTurn PlayerSpot
  | -- | Player puts a card from his hand on its part of the board
    Place PlayerSpot CardSpot (Card Core)

reportEffect ::
  MonadWriter (Board UI) m =>
  PlayerSpot ->
  CardSpot ->
  AttackEffect ->
  m ()
reportEffect pSpot cSpot effect =
  tell $ Board {playerTop = pTop, playerBottom = pBot}
  where
    effectfull = AttackEffects $ Map.singleton cSpot effect
    effectless = AttackEffects Map.empty
    (botInPlace, topInPlace) =
      case pSpot of
        PlayerBottom -> (effectfull, effectless)
        PlayerTop -> (effectless, effectfull)
    pTop :: PlayerPart UI = PlayerPart topInPlace () () ()
    pBot :: PlayerPart UI = PlayerPart botInPlace () () ()

play :: Board Core -> GamePlayEvent-> Either Text (Board Core, Board UI)
play board action =
  playM board action
    & runWriterT
    & runExcept

playM ::
  MonadError Text m =>
  MonadWriter (Board UI) m =>
  Board Core ->
  GamePlayEvent ->
  m (Board Core)
playM board (EndTurn pSpot) = endTurn board pSpot
playM board (Place pSpot cSpot (card :: Card Core))
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
    pLens = spotToLens pSpot
    base :: PlayerPart Core = board ^. pLens
    hand :: [Card Core] = boardToHand board $ spotToLens pSpot
    hand' :: [Card Core] = delete card hand
    onTable :: Map CardSpot (Creature Core) = inPlace base
    onTable' = onTable & at cSpot ?~ unsafeCardToCreature card
    playerPart' = base {inPlace = onTable', inHand = hand'}

endTurn ::
  MonadWriter (Board UI) m =>
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
  MonadWriter (Board UI) m =>
  Board Core ->
  PlayerSpot ->
  CardSpot ->
  m (Board Core)
attack board pSpot cSpot =
  case (attacker, allyBlocker, attackee) of
    (_, Just _, _) -> return board -- an ally blocks the way
    (Just hitter, _, Just (hitSpot, hittee)) ->
      -- attack can proceed
      let effect = singleAttack hitter hittee
          newHittee = applyAttackEffect effect hittee
       in do
            reportEffect pSpot cSpot $ -- attacker bumps
              createAttackEffect Nothing (Just True) Nothing
            reportEffect (otherPlayerSpot pSpot) hitSpot effect -- hittee
            return (board & pOtherSpotLens . #inPlace . at hitSpot .~ newHittee)
    _ -> return board -- no attacker or nothing to attack
  where
    pSpotLens = spotToLens pSpot
    pOtherSpotLens :: Lens' (Board Core) (PlayerPart Core)
    pOtherSpotLens = spotToLens $ otherPlayerSpot pSpot
    attackersInPlace :: Map CardSpot (Creature Core) =
      board ^. pSpotLens . #inPlace
    attackeesInPlace :: Map CardSpot (Creature Core) =
      board ^. pOtherSpotLens . #inPlace
    attacker :: Maybe (Creature Core) = attackersInPlace !? cSpot
    attackerSkills :: [Skill] = attacker >>= skills & fromMaybe []
    allyBlocker :: Maybe (Creature Core) =
      if any (`elem` attackerSkills) [Ranged, HitFromBack]
        then Nothing -- attacker bypasses ally blocker (if any)
        else allyBlockerSpot cSpot >>= (attackersInPlace !?)
    attackedSpots :: [CardSpot] = enemySpots attackerSkills cSpot
    -- For the moment a card attacks the first card in front of it. If
    -- later there's a skill Rampage, this will change:
    attackee :: Maybe (CardSpot, Creature Core) =
      asum [(spot,) <$> (attackeesInPlace !? spot) | spot <- attackedSpots]

applyAttackEffect :: AttackEffect -> Creature Core -> Maybe (Creature Core)
applyAttackEffect effect creature@Creature {..} =
  case effect of
    AttackEffect {death = True} -> Nothing
    AttackEffect {hitPointsChange = i} -> Just $ creature {hp = hp + i}

-- The effect of an attack on the defender
singleAttack :: Creature Core -> Creature Core -> AttackEffect
singleAttack attacker defender
  | hps' <= 0 = createAttackEffect (Just True) Nothing Nothing
  | otherwise = createAttackEffect Nothing Nothing (Just $ - hit)
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

-- | The other spot in the column in the spot's part
-- otherYSpot :: CardSpot -> CardSpot
-- otherYSpot TopLeft = BottomLeft
-- otherYSpot Top = Bottom
-- otherYSpot TopRight = BottomRight
-- otherYSpot BottomLeft = TopLeft
-- otherYSpot Bottom = Top
-- otherYSpot BottomRight = TopRight

-- | All enemy spots of a spot
allEnemySpots :: CardSpot -> [CardSpot]
allEnemySpots = enemySpots [Ranged]

-- | Spots that can be attacked from a spot
-- | Spot as argument is in one player part while spots returned
-- | are in the other player part.
-- | The order in the result matters, the first element is the first spot
-- | attacked, then the second element is attacked if the first spot is empty
enemySpots :: [Skill] -> CardSpot -> [CardSpot]
enemySpots skills cSpot =
  map bottomSpotOfTopVisual $
    if
        | Ranged `elem` skills -> spotsInSight
        | inTheBack cSpot -> if HitFromBack `elem` skills then take 1 spotsInSight else []
        | otherwise -> take 1 spotsInSight
  where
    spotsInSight =
      case cSpot of
        TopLeft -> [TopLeft, BottomLeft]
        Top -> [Top, Bottom]
        TopRight -> [TopRight, BottomRight]
        BottomLeft -> [TopLeft, BottomLeft]
        Bottom -> [Top, Bottom]
        BottomRight -> [TopRight, BottomRight]

-- | The order in which cards attack
attackOrder :: [CardSpot]
attackOrder = [BottomRight, Bottom, BottomLeft, TopRight, Top, TopLeft]
