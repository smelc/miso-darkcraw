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
    attackOrder, -- exported for tests only
    nextAttackSpot,
    enemySpots,
    GamePlayEvent (..),
    play,
    playAll,
    playM,
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
import Data.List (elemIndex)
import Data.List.Index (deleteAt)
import Data.Map.Strict ((!?), Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

data GamePlayEvent
  = -- | A player finishes its turn, resolving it in a single card spot
    EndTurn PlayerSpot CardSpot
  | -- | A Nothing case, for convenience
    NoPlayEvent
  | -- | Player puts a card from his hand on its part of the board
    Place PlayerSpot CardSpot HandIndex
  deriving (Eq, Show)

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

play :: Board Core -> GamePlayEvent -> Either Text (Board Core, Board UI)
play board action =
  playM board action
    & runWriterT
    & runExcept

playAll :: Board Core -> [GamePlayEvent] -> Either Text (Board Core, Board UI)
playAll board [] = Right (board, mempty)
playAll board (e : events) = do
  (board', boardui') <- play board e
  (board'', boardui'') <- playAll board' events
  return (board'', boardui' <> boardui'')

playM ::
  MonadError Text m =>
  MonadWriter (Board UI) m =>
  Board Core ->
  GamePlayEvent ->
  m (Board Core)
playM board (EndTurn pSpot cSpot) = Game.attack board pSpot cSpot
playM board NoPlayEvent = return board
playM board (Place pSpot cSpot (handhi :: HandIndex)) = do
  card <- lookupHand hand handi
  let onTable :: Map CardSpot (Creature Core) = inPlace base
  if Map.member cSpot onTable
    then throwError $ "Cannot place card on non-empty spot: " <> Text.pack (show cSpot)
    else do
      let inPlace' = Map.insert cSpot (unsafeCardToCreature card) onTable
      let part' = base {inPlace = inPlace', inHand = hand'}
      return $ boardSetPart board pSpot part'
  where
    handi = unHandIndex handhi
    pLens = spotToLens pSpot
    base :: PlayerPart Core = board ^. pLens
    hand :: [Card Core] = boardToHand board $ spotToLens pSpot
    hand' :: [Card Core] = deleteAt handi hand

-- | Events to schedule when a turn is finished, either when the player
-- | pressed "End Turn" or when the AI is finished.
allEndTurnEvents :: PlayerSpot -> [GamePlayEvent]
allEndTurnEvents pSpot = map (EndTurn pSpot) $ attackOrder pSpot

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
attackOrder :: PlayerSpot -> [CardSpot]
attackOrder PlayerTop =
  [BottomRight, Bottom, BottomLeft, TopRight, Top, TopLeft]
attackOrder PlayerBottom =
  map bottomSpotOfTopVisual $ reverse $ attackOrder PlayerTop

nextAttackSpot :: Board Core -> PlayerSpot -> Maybe CardSpot -> Maybe CardSpot
nextAttackSpot board pSpot cSpot =
  case cSpot of
    Nothing -> find hasCreature spots
    Just cSpot ->
      let idx = elemIndex cSpot spots
       in case idx of
            Nothing -> error "wrong use of nextAttackSpot"
            Just idx ->
              let spots' = splitAt idx spots & snd & drop 1
               in find hasCreature spots'
  where
    spots :: [CardSpot] = attackOrder pSpot
    hasCreature c = isJust $ boardToInPlaceCreature board (spotToLens pSpot) c
