{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
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
    drawCards,
    transferCards,
  )
where

import Board
import Card
import Constants
import Control.Exception (assert)
import Control.Lens
import Control.Monad.Except
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable
import Data.List (elemIndex)
import Data.List.Index (deleteAt)
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import SharedModel
import System.Random.Shuffle (shuffleM)

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
  InPlaceEffect ->
  m ()
reportEffect pSpot cSpot effect =
  tell $ Board {playerTop = pTop, playerBottom = pBot}
  where
    effectfull = InPlaceEffects $ Map.singleton cSpot effect
    effectless = InPlaceEffects Map.empty
    (botInPlace, topInPlace) =
      case pSpot of
        PlayerBottom -> (effectfull, effectless)
        PlayerTop -> (effectless, effectfull)
    pTop :: PlayerPart UI = mempty {inPlace = topInPlace}
    pBot :: PlayerPart UI = mempty {inPlace = botInPlace}

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
      reportEffect pSpot cSpot $ mempty {fadeIn = True}
      return $ boardSetPart board pSpot part'
  where
    handi = unHandIndex handhi
    pLens = spotToLens pSpot
    base :: PlayerPart Core = board ^. pLens
    hand :: [Card Core] = boardToHand board pSpot
    hand' :: [Card Core] = deleteAt handi hand

drawCards ::
  SharedModel ->
  Board Core ->
  -- | The player drawing cards
  PlayerSpot ->
  -- | The number of cards to draw
  Int ->
  Either Text (Board Core, Board UI, SharedModel)
drawCards shared board _ 0 = return (board, mempty, shared)
drawCards shared board pSpot i = do
  (board', boardui, shared') <- drawCard shared board pSpot
  (board'', boardui', shared'') <- drawCards shared' board' pSpot (i - 1)
  return (board'', boardui <> boardui', shared'')

drawCard ::
  SharedModel ->
  Board Core ->
  -- | The player drawing cards
  PlayerSpot ->
  Either Text (Board Core, Board UI, SharedModel)
drawCard shared board pSpot =
  drawCardM board pSpot
    & runWriterT
    & flip runStateT shared
    & runExcept
    & fmap flatten
  where
    flatten ((x, y), z) = (x, y, z)

drawCardM ::
  MonadError Text m =>
  MonadWriter (Board UI) m =>
  MonadState SharedModel m =>
  Board Core ->
  PlayerSpot ->
  m (Board Core)
drawCardM board pSpot =
  case assert (bound >= 0) bound of
    0 -> return board
    _ -> do
      let stack :: [CardIdentifier] = boardToStack board pSpot
      let hand :: [Card Core] = boardToHand board pSpot
      stdgen <- use #sharedStdGen
      let (idrawn, stdgen') = randomR (0, bound - 1) stdgen
      #sharedStdGen .= stdgen'
      let drawn :: CardIdentifier = stack !! idrawn
      shared <- get
      let maybeDrawnCard = identToCard shared drawn
      when (isNothing maybeDrawnCard) $ throwError $ Text.pack $ "Identifier " ++ show drawn ++ " cannot be mapped to Card"
      let drawnCard = fromJust maybeDrawnCard & unliftCard
      let stack' = deleteAt idrawn stack
      let hand' = hand ++ [drawnCard]
      tell $ boardAddToHand mempty pSpot $ length hand
      let board' = boardSetStack board pSpot stack'
      let board'' = boardSetHand board' pSpot hand'
      return board''
  where
    bound = nbCardsToDraw board pSpot

transferCards ::
  SharedModel ->
  Board Core ->
  PlayerSpot ->
  (SharedModel, Board Core, Board UI)
transferCards shared@SharedModel {sharedStdGen} board pSpot =
  (shared {sharedStdGen = stdgen'}, board', boardui')
  where
    (board', boardui', stdgen') =
      transferCards' sharedStdGen board pSpot

transferCards' ::
  StdGen ->
  Board Core ->
  PlayerSpot ->
  (Board Core, Board UI, StdGen)
transferCards' stdgen board pSpot =
  transferCardsM board pSpot & flip runRandT stdgen & runWriter & reorg
  where
    reorg ((b, s), bui) = (b, bui, s)

-- | Transfer cards from the discarded stack to the other stack
transferCardsM ::
  MonadRandom m =>
  MonadWriter (Board UI) m =>
  Board Core ->
  PlayerSpot ->
  m (Board Core)
transferCardsM board pSpot =
  if not needTransfer
    then pure board
    else do
      tell $ boardSetDiscarded mempty pSpot (- length discarded)
      tell $ boardSetStack mempty pSpot (length discarded)
      discarded' <- shuffleM discarded
      let part' = part {discarded = [], stack = stack ++ discarded'}
      return $ boardSetPart board pSpot part'
  where
    (hand, actualHandSize) = (boardToHand board pSpot, length hand)
    cardsToDraw = assert (actualHandSize <= initialHandSize) $ maxHandSizeAtRefill - actualHandSize
    (stack, stackSize) = (boardToStack board pSpot, length stack)
    needTransfer = cardsToDraw > stackSize
    discarded = boardToDiscarded board pSpot
    part = boardToPart board pSpot

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
          board' = applyInPlaceEffectOnBoard effect board (attackeePSpot, hitSpot, hittee)
       in do
            reportEffect pSpot cSpot $ mempty {attackBump = True}
            reportEffect attackeePSpot hitSpot effect -- hittee
            return board'
    (Just hitter, _, Nothing) -> do
      -- nothing to attack, contribute to the score!
      let hit = Card.attack hitter
      reportEffect pSpot cSpot $ mempty {attackBump = True, scoreChange = hit}
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

applyInPlaceEffectOnBoard ::
  -- | The effect of the attacker on the hittee
  InPlaceEffect ->
  -- | The input board
  Board Core ->
  -- | The creature being hit
  (PlayerSpot, CardSpot, Creature Core) ->
  -- | The updated board
  Board Core
applyInPlaceEffectOnBoard effect board (pSpot, cSpot, hittee) =
  case hittee' of
    Just _ -> board'
    Nothing ->
      board' & spotToLens pSpot . #discarded <>~ [creatureToIdentifier hittee]
  where
    hittee' = applyInPlaceEffect effect hittee
    -- Update the hittee in the board, putting Nothing or Just _:
    board' = board & spotToLens pSpot . #inPlace . at cSpot .~ hittee'

applyInPlaceEffect ::
  -- | The effect of the attacker on the hittee
  InPlaceEffect ->
  -- | The creature being hit
  Creature Core ->
  -- | The creature being hit, after applying the effect; or None if dead
  Maybe (Creature Core)
applyInPlaceEffect effect creature@Creature {..} =
  case effect of
    InPlaceEffect {death = True} -> Nothing
    InPlaceEffect {hitPointsChange = i} -> Just $ creature {hp = hp + i}

-- The effect of an attack on the defender
singleAttack :: Creature Core -> Creature Core -> InPlaceEffect
singleAttack attacker defender
  | hps' <= 0 = mempty {death = True}
  | otherwise = mempty {hitPointsChange = - hit}
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
    hasCreature c = isJust $ boardToInPlaceCreature board pSpot c

nbCardsToDraw :: Board Core -> PlayerSpot -> Int
nbCardsToDraw board pSpot =
  assert (0 <= result' && result' <= maxHandSizeAtRefill) result'
  where
    handLen = length $ boardToHand board pSpot
    stackLen = length $ boardToStack board pSpot
    result = min (maxHandSizeAtRefill - handLen) stackLen
    result' = max 0 result -- Needed because initial hand size is greater
    -- than maxHandSizeAtRefill
