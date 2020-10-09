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
    nbCardsToDraw,
  )
where

import Board
import Card
import Constants
import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable
import Data.List (elemIndex)
import Data.List.Index (deleteAt)
import Data.Map.Strict ((!?), Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import SharedModel
import System.Random

data GamePlayEvent
  = -- | A player finishes its turn, resolving it in a single card spot.
    -- | This event is a bit tricky to handle, because every consumer should
    -- | take care of scheduling the next event (if any) using 'nextAttackSpot'
    -- | or scheduling 'GameIncrTurn' if none.
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
    pTop :: PlayerPart UI = PlayerPart topInPlace () () () ()
    pBot :: PlayerPart UI = PlayerPart botInPlace () () () ()

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
playM board gpe =
  playM' board gpe

playM' ::
  MonadError Text m =>
  MonadWriter (Board UI) m =>
  Board Core ->
  GamePlayEvent ->
  m (Board Core)
playM' board (EndTurn pSpot cSpot) = Game.attack board pSpot cSpot
playM' board NoPlayEvent = return board
playM' board (Place pSpot cSpot (handhi :: HandIndex)) = do
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
    hand :: [Card Core] = boardToHand board pSpot
    hand' :: [Card Core] = deleteAt handi hand

drawCard ::
  MonadError Text m =>
  MonadWriter (Board UI) m =>
  MonadState StdGen m =>
  SharedModel ->
  Board Core ->
  PlayerSpot ->
  m (Board Core)
drawCard shared board pSpot =
  case bound of
    0 -> return board
    _ -> do
      stdgen <- get
      let stack :: [CardIdentifier] = boardToStack board pSpot
      let hand :: [Card Core] = boardToHand board pSpot
      let (idrawn, stdgen') = randomR (0, bound - 1) stdgen
      put stdgen'
      let drawn :: CardIdentifier = stack !! idrawn
      let maybeDrawnCard = identToCard shared drawn
      when (isNothing maybeDrawnCard) $ throwError $ Text.pack $ "Identifier " ++ show drawn ++ " cannot be mapped to Card"
      let drawnCard = fromJust maybeDrawnCard & unliftCard
      let stack' = deleteAt idrawn stack
      let hand' = hand ++ [drawnCard]
      let board' = boardSetStack board pSpot stack'
      let board'' = boardSetHand board' pSpot hand'
      return board''
  where
    bound = nbCardsToDraw board pSpot

-- | Card at [pSpot],[cSpot] attacks; causing changes to a board
attack ::
  MonadWriter (Board UI) m =>
  Board Core ->
  PlayerSpot ->
  CardSpot ->
  m (Board Core)
attack board pSpot cSpot =
  case (attacker, allyBlocker, attackee) of
    (Nothing, _, _) -> return board -- no attacker
    (_, Just _, _) -> return board -- an ally blocks the way
    (Just hitter, _, Just (hitSpot, hittee)) ->
      -- attack can proceed
      let effect = singleAttack hitter hittee
          board' = applyAttackEffectOnBoard effect board (attackeePSpot, hitSpot, hittee)
       in do
            reportEffect pSpot cSpot $ -- attacker bumps
              createAttackEffect Nothing (Just True) Nothing Nothing
            reportEffect attackeePSpot hitSpot effect -- hittee
            return board'
    (Just hitter, _, Nothing) -> do
      -- nothing to attack, contribute to the score!
      let hit = Card.attack hitter
      reportEffect pSpot cSpot $
        createAttackEffect Nothing (Just True) Nothing (Just hit)
      return (board & spotToLens pSpot . #score +~ hit)
  where
    pSpotLens = spotToLens pSpot
    attackeePSpot = otherPlayerSpot pSpot
    pOtherSpotLens :: Lens' (Board Core) (PlayerPart Core)
    pOtherSpotLens = spotToLens attackeePSpot
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

applyAttackEffectOnBoard ::
  -- | The effect of the attacker on the hittee
  AttackEffect ->
  -- | The input board
  Board Core ->
  -- | The creature being hit
  (PlayerSpot, CardSpot, Creature Core) ->
  -- | The updated board
  Board Core
applyAttackEffectOnBoard effect board (pSpot, cSpot, hittee) =
  case hittee' of
    Just _ -> board'
    Nothing ->
      board' & spotToLens pSpot . #discarded <>~ [creatureToIdentifier hittee]
  where
    hittee' = applyAttackEffect effect hittee
    -- Update the hittee in the board, putting Nothing or Just _:
    board' = board & spotToLens pSpot . #inPlace . at cSpot .~ hittee'

applyAttackEffect ::
  -- | The effect of the attacker on the hittee
  AttackEffect ->
  -- | The creature being hit
  Creature Core ->
  -- | The creature being hit, after applying the effect; or None if dead
  Maybe (Creature Core)
applyAttackEffect effect creature@Creature {..} =
  case effect of
    AttackEffect {death = True} -> Nothing
    AttackEffect {hitPointsChange = i} -> Just $ creature {hp = hp + i}

-- The effect of an attack on the defender
singleAttack :: Creature Core -> Creature Core -> AttackEffect
singleAttack attacker defender
  | hps' <= 0 = createAttackEffect (Just True) Nothing Nothing Nothing
  | otherwise = createAttackEffect Nothing Nothing (Just $ - hit) Nothing
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

nbCardsToDraw :: Board Core -> PlayerSpot -> Int
nbCardsToDraw board pSpot =
  min (handSize - length hand) (length stack)
  where
    hand = boardToHand board pSpot
    stack = board ^. (spotToLens pSpot . #stack)
