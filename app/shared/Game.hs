{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
    Result (..),
    play,
    playAll,
    playM,
    nbCardsToDraw,
    drawCards,
    transferCards,
    PlayTarget (..),
    Game.appliesTo,
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
import GHC.Generics (Generic)
import SharedModel (SharedModel, unsafeIdentToCard)
import qualified SharedModel
import System.Random.Shuffle (shuffleM)

-- | On what a card can be applied
data PlayTarget
  = -- | Neutral card applies to all in place cards of a player
    PlayerTarget PlayerSpot
  | -- | Creature card placed at given spot
    -- or Neutral card applies to a given in place card of a player
    CardTarget PlayerSpot CardSpot
  deriving (Eq, Generic, Show)

-- FIXME @smelc rename me into PlayEvent and use me qualified in imports
data GamePlayEvent
  = -- | A player finishes its turn, resolving it in a single card spot.
    -- | This event is a bit tricky to handle, because every consumer should
    -- | take care of scheduling the next event (if any) using 'nextAttackSpot'
    -- | or scheduling 'GameIncrTurn' if none.
    EndTurn PlayerSpot CardSpot
  | -- | A Nothing case, for convenience
    NoPlayEvent
  | -- | Player puts a card from his hand on its part of the board
    Place PlayTarget HandIndex
  | -- | AI puts a card from his hand. This constructor has better
    -- testing behavior than 'Place': it makes the generated events commute.
    Place' PlayTarget CreatureID
  deriving (Eq, Show)

data Result = Result (Board Core) (Board UI)

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

play :: SharedModel -> Board Core -> GamePlayEvent -> Either Text Result
play shared board action =
  playM shared board action
    & runWriterT
    & runExcept
    & build
  where
    build (Left err) = Left err
    build (Right (b, a)) = Right $ Result b a

playAll :: SharedModel -> Board Core -> [GamePlayEvent] -> Either Text Result
playAll _ board [] = Right $ Result board mempty
playAll shared board (e : events) = do
  Result board' boardui' <- play shared board e
  Result board'' boardui'' <- playAll shared board' events
  return $ Result board'' (boardui' <> boardui'')

playM ::
  MonadError Text m =>
  MonadWriter (Board UI) m =>
  SharedModel ->
  Board Core ->
  GamePlayEvent ->
  m (Board Core)
playM _ board (EndTurn pSpot cSpot) = Game.attack board pSpot cSpot
playM _ board NoPlayEvent = return board
playM shared board (Place pTarget (handhi :: HandIndex)) = do
  ident <- lookupHand hand handi
  let card = unsafeIdentToCard shared ident & unliftCard
  let onTable :: Map CardSpot (Creature Core) = inPlace base
  case (Map.member cSpot onTable, cardToCreature card) of
    (True, _) -> throwError $ "Cannot place card on non-empty spot: " <> Text.pack (show cSpot)
    (_, Nothing) -> throwError $ Text.pack $ "Cannot place non-creature card. Received: " ++ show card
    (_, Just card) -> do
      let inPlace' = Map.insert cSpot card onTable
      let part' = base {inPlace = inPlace', inHand = hand'}
      reportEffect pSpot cSpot $ mempty {fadeIn = True}
      return $ boardSetPart board pSpot part'
  where
    handi = unHandIndex handhi
    pLens = spotToLens pSpot
    base :: PlayerPart Core = board ^. pLens
    hand = boardToHand board pSpot
    hand' = deleteAt handi hand
    (pSpot, cSpot) =
      case pTarget of
        PlayerTarget _ -> error "PlayerTarget unsupported"
        CardTarget p c -> (p, c)
playM shared board (Place' pTarget creatureID) =
  case idx of
    Nothing -> throwError $ Text.pack $ "Card not found in " ++ show pSpot ++ ": " ++ show creatureID
    Just i -> playM shared board (Place pTarget i)
  where
    pSpot =
      case pTarget of
        PlayerTarget p -> p
        CardTarget p _ -> p
    idxAndIDs =
      boardToHand board pSpot
        & map identToId
        & zip [HandIndex 0 ..]
    idx = find (\(_, m) -> m == Just creatureID) idxAndIDs <&> fst

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
      let hand = boardToHand board pSpot
      stdgen <- use #sharedStdGen
      let (idrawn, stdgen') = randomR (0, bound - 1) stdgen
      #sharedStdGen .= stdgen'
      let ident :: CardIdentifier = stack !! idrawn
      let stack' = deleteAt idrawn stack
      let hand' = hand ++ [ident]
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
transferCards shared board pSpot =
  (SharedModel.withStdGen shared stdgen', board', boardui')
  where
    (board', boardui', stdgen') =
      transferCards' (SharedModel.getStdGen shared) board pSpot

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

-- | board n pSpot target holds iff player at 'pSpot' can play card 'n'
-- on 'target'
appliesTo :: Board Core -> Neutral -> PlayerSpot -> PlayTarget -> Bool
appliesTo board n playingPlayer target =
  case (target, n) of
    (PlayerTarget pSpot, InfernalHaste)
      | pSpot == playingPlayer -> True
    (CardTarget pSpot cSpot, Health)
      | pSpot == playingPlayer -> Board.appliesTo n board pSpot cSpot
    (CardTarget pSpot cSpot, Life)
      | pSpot == playingPlayer -> Board.appliesTo n board pSpot cSpot
    (_, _) -> False

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
    attackerSkills :: [Skill] = attacker <&> skills & fromMaybe []
    allyBlocker :: Maybe (Creature Core) =
      if any (`elem` attackerSkills) [Ranged, LongReach]
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
        | inTheBack cSpot -> if LongReach `elem` skills then take 1 spotsInSight else []
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
