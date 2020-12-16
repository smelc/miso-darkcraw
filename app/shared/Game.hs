{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
    Event (..),
    PolyResult (..),
    Result (..),
    placePrimeToHandIndex,
    play,
    playAll,
    playM,
    nbCardsToDraw,
    drawCards,
    transferCards,
    Target (..),
    Game.appliesTo,
  )
where

import Board
import Card hiding (ID)
import qualified Card
import Constants
import Control.Exception (assert)
import Control.Lens
import Control.Monad.Except
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Bifunctor as Bifunctor
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
data Target
  = -- | Neutral card applies to all in place cards of a player
    PlayerTarget PlayerSpot
  | -- | Creature card placed at given spot
    -- or Neutral card applies to a given in place card of a player
    CardTarget PlayerSpot CardSpot
  deriving (Eq, Generic, Show)

targetToPlayerSpot :: Target -> PlayerSpot
targetToPlayerSpot (PlayerTarget pSpot) = pSpot
targetToPlayerSpot (CardTarget pSpot _) = pSpot

data Event
  = -- | A card attacks at the given spot. The first Boolean indicates
    -- whether the next spot (as defined by 'nextAttackSpot') should
    -- be enqueued after solving this attack. The second Boolean indicates
    -- whether 'GameIncrTurn' (change player turn) should be performed
    -- after solving this attack.
    Attack PlayerSpot CardSpot Bool Bool
  | -- | A Nothing case, for convenience
    NoPlayEvent
  | -- | Player puts a card from his hand on its part of the board
    Place Target HandIndex
  | -- | AI puts a card from his hand. This constructor has better
    -- testing behavior than 'Place': it makes the generated events commute.
    Place' Target Card.ID
  deriving (Eq, Generic, Show)

-- | The polymorphic version of 'Result'. Used for implementors that
-- do not return events and hence instantiate 'a' by ()
data PolyResult a = Result (Board Core) a (Board UI)
  deriving (Eq, Show)

-- | The result of playing an 'Event': an updated board, the next event
-- (if any), and the animations
type Result = PolyResult (Maybe Event)

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
        PlayerBot -> (effectfull, effectless)
        PlayerTop -> (effectless, effectfull)
    pTop :: PlayerPart UI = mempty {inPlace = topInPlace}
    pBot :: PlayerPart UI = mempty {inPlace = botInPlace}

play :: SharedModel -> Board Core -> Event -> Either Text Result
play shared board action =
  playM shared board action
    & runWriterT
    & runExcept
    & build
  where
    build (Left err) = Left err
    build (Right ((b, es), a)) = Right $ Result b es a

-- | Please avoid calling this function if you can. Its effect is
-- difficult to predict because of cards that create events, as these
-- events are played right away (instead of being enqueued in the main loop
-- like when playing Infernal Haste interactively). FIXME @smelc
-- take an additional parameter telling whether to mimic the main loop
-- (for Game.Attack events).
playAll :: SharedModel -> Board Core -> [Event] -> Either Text (PolyResult ())
playAll shared board es = go board mempty es
  where
    go b anims [] = Right $ Result b () anims
    go b anims (e : tl) = do
      Result board' new anims' <- play shared b e
      -- /!\ We're enqueueing the event created by playing 'e' i.e. 'new',
      -- before the rest of the events ('tl'). This means 'tl' will be played in a state
      -- that is NOT the one planned when building 'e : tl' (except if the
      -- caller is very smart). We have no way around that: this is caused
      -- by Infernal Haste that creates events when playing it. But this means
      -- we shouldn't be too hard with events that yield failures. Maybe
      -- they are being played in an unexpected context (imagine if
      -- Infernal Haste clears the opponent's board, next spells may be useless).
      go board' (anims <> anims') $ maybeToList new ++ tl

playM ::
  MonadError Text m =>
  MonadWriter (Board UI) m =>
  SharedModel ->
  Board Core ->
  Event ->
  m (Board Core, Maybe Event)
playM _ board (Attack pSpot cSpot _ _) = do
  board' <- Game.attack board pSpot cSpot
  return (board', Nothing)
playM _ board NoPlayEvent = return (board, Nothing)
playM shared board (Place target (handhi :: HandIndex)) = do
  ident <- lookupHand hand handi
  let card = unsafeIdentToCard shared ident & unliftCard
  case (target, card) of
    (CardTarget pSpot cSpot, CreatureCard card) -> do
      board'' <- playCardTargetM board' pSpot cSpot card
      return (board'', Nothing)
    (_, NeutralCard NeutralObject {neutral}) ->
      playPlayerTargetM board' playingPlayer target neutral
    _ -> throwError $ Text.pack $ "Wrong (Target, card) combination: (" ++ show target ++ ", " ++ show card ++ ")"
  where
    handi = unHandIndex handhi
    (hand, hand') = (boardToHand board pSpot, deleteAt handi hand)
    board' = boardSetHand board pSpot hand'
    pSpot =
      -- FIXME @smelc Change me, this will be wrong once there are neutral
      -- cards that are applied on the opponent's board. GameModel's
      -- playingPlayer should be passed instead
      case target of PlayerTarget p -> p; CardTarget p _ -> p
    playingPlayer = pSpot
playM shared board e@(Place' target id) =
  case placePrimeToHandIndex board e of
    Nothing -> throwError $ Text.pack $ "Card not found in " ++ show pSpot ++ ": " ++ show id
    Just i -> playM shared board (Place target i)
  where
    pSpot = targetToPlayerSpot target

placePrimeToHandIndex :: Board 'Core -> Event -> Maybe HandIndex
placePrimeToHandIndex board =
  \case
    Place' target id -> f target id
    _ -> Nothing
  where
    idxAndIDs pSpot =
      boardToHand board pSpot & zip [HandIndex 0 ..]
    f target id =
      find (\(_, m) -> m == id) (idxAndIDs $ targetToPlayerSpot target) <&> fst

playPlayerTargetM ::
  MonadError Text m =>
  MonadWriter (Board UI) m =>
  Board Core ->
  PlayerSpot ->
  Target ->
  Neutral ->
  m (Board Core, Maybe Event)
playPlayerTargetM board _playingPlayer target n =
  case (n, target) of
    (InfernalHaste, PlayerTarget pSpot) ->
      return (board, event)
      where
        event =
          nextAttackSpot board pSpot Nothing
            <&> (\cSpot -> Attack pSpot cSpot True False)
    (Health, CardTarget pSpot cSpot) -> do
      board' <- addHitpoints pSpot cSpot 1
      return (board', Nothing)
    (Life, CardTarget pSpot cSpot) -> do
      board' <- addHitpoints pSpot cSpot 3
      return (board', Nothing)
    _ -> throwError $ Text.pack $ "Wrong (Target, Neutral) combination: (" ++ show target ++ ", " ++ show n ++ ")"
  where
    addHitpoints pSpot cSpot hps =
      case boardToInPlaceCreature board pSpot cSpot of
        Nothing -> return board
        Just c@Creature {hp} ->
          -- FIXME @smelc Generate animation showing hit points increase
          return board'
          where
            c' = c {hp = hp + hps}
            board' = boardSetCreature board pSpot cSpot c'

playCardTargetM ::
  MonadError Text m =>
  MonadWriter (Board UI) m =>
  Board Core ->
  PlayerSpot ->
  CardSpot ->
  Creature Core ->
  m (Board Core)
playCardTargetM board pSpot cSpot creature = do
  when (Map.member cSpot onTable) $ throwError $ "Cannot place card on non-empty spot: " <> Text.pack (show cSpot)
  let inPlace' =
        -- Left-biased union
        Map.union
          (Map.fromList disciplinedNeighbors')
          (Map.insert cSpot creature onTable)
  -- FIXME @smelc Record animations for discipline boost
  let part' = base {inPlace = inPlace'}
  reportEffect pSpot cSpot $ mempty {fadeIn = True}
  return $ boardSetPart board pSpot part'
  where
    base :: PlayerPart Core = boardToPart board pSpot
    onTable :: Map CardSpot (Creature Core) = inPlace base
    hasDiscipline c = Discipline `elem` skills c
    disciplinedNeighbors =
      boardToNeighbors board pSpot cSpot Board.Cardinal
        & map liftDisciplinedJust
        & catMaybes
    liftDisciplinedJust (cSpot, Just c) | hasDiscipline c = Just (cSpot, c)
    liftDisciplinedJust _ = Nothing
    applyDisciplineBoost :: Creature 'Core -> Creature 'Core
    applyDisciplineBoost Creature {..} = Creature {hp = hp + 1, attack = attack + 1, ..}
    disciplinedNeighbors' =
      map (Bifunctor.second applyDisciplineBoost) disciplinedNeighbors

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
      let stack :: [Card.ID] = boardToStack board pSpot
      let hand = boardToHand board pSpot
      stdgen <- use #sharedStdGen
      let (idrawn, stdgen') = randomR (0, bound - 1) stdgen
      #sharedStdGen .= stdgen'
      let ident :: Card.ID = stack !! idrawn
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
appliesTo :: Board Core -> Neutral -> PlayerSpot -> Target -> Bool
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
attackOrder PlayerBot =
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
