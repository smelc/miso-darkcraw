{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Board
  ( allCardsSpots,
    allPlayersSpots,
    AttackEffect (..),
    AttackEffects (..),
    bottomSpotOfTopVisual,
    boardSetPart,
    boardToHoleyInPlace,
    boardToInHandCreaturesToDraw,
    boardToInPlaceCreature,
    boardToHand,
    boardToPart,
    Board (..),
    CardSpot (..),
    createAttackEffect,
    endingPlayerSpot,
    exampleBoard,
    HandIndex (..),
    inTheBack,
    InHandType (..),
    lookupHand,
    otherPlayerSpot,
    PlayerPart (..),
    PlayerSpot (..),
    StackType (..),
    startingPlayerSpot,
    spotToLens,
    boardToStack,
    boardSetStack,
    boardSetHand,
    boardAddToHand,
    emptyBoard,
    boardSetCreature,
    boardToDiscarded,
    boardSetDiscarded,
  )
where

import Card
import Constants (handSize)
import Control.Lens
import Control.Monad.Except (MonadError, throwError)
import Data.Generics.Labels ()
import Data.Kind (Constraint, Type)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import Formatting (hex, sformat, (%))
import GHC.Generics (Generic)

-- | The spot of a card, as visible from the top of the screen. For the
-- | bottom part, think as if it was in the top, turning the board
-- | 180 degrees clockwise; or use these values and map [bottomSpotOfTopVisual].
data CardSpot
  = TopLeft
  | Top
  | TopRight
  | BottomLeft
  | Bottom
  | BottomRight
  deriving (Enum, Eq, Ord, Show, Generic)

allCardsSpots :: [CardSpot]
allCardsSpots = [TopLeft ..]

type CardsOnTable = Map.Map CardSpot (Creature Core)

-- | A convenience constructor to create the bottom part of a board
-- | by using the CardSpot that you see instead of having to consider
-- | the 180 degrees rotation mentioned in CardSpot
makeBottomCardsOnTable :: CardsOnTable -> CardsOnTable
makeBottomCardsOnTable =
  Map.mapKeys bottomSpotOfTopVisual

-- | Returns a bottom position, by taking a position that makes sense visually
-- | I.e. if you give this method [TopLeft], it'll correspond to the [TopLeft]
-- | bottom position that you SEE; even if positions make sense for the top
-- | part. This method takes care of translating correctly.
bottomSpotOfTopVisual :: CardSpot -> CardSpot
bottomSpotOfTopVisual = \case
  TopLeft -> BottomRight
  Top -> Bottom
  TopRight -> BottomLeft
  BottomLeft -> TopRight
  Bottom -> Top
  BottomRight -> TopLeft

-- It is a bit unfortunate to have these types defined here
-- as they are UI only. However we need them to define the InPlaceType family

data AttackEffect = AttackEffect
  { -- | Creature dies
    death :: Bool,
    -- | Creature attacked (value used solely for animations)
    attackBump :: Bool,
    -- | Hits points changed
    hitPointsChange :: Int,
    -- | Score changed
    scoreChange :: Int
  }
  deriving (Eq, Generic, Show)

-- | How to build instances of [AttackEffect] values
createAttackEffect ::
  -- | The [death] field
  Maybe Bool ->
  -- | The [attackBump] field
  Maybe Bool ->
  -- | The [hitPointsChange] field
  Maybe Int ->
  -- | The [scoreChange] field
  Maybe Int ->
  AttackEffect
createAttackEffect mDeath mAttackBump mHitPointsChange mScoreChange =
  AttackEffect
    (fromMaybe (death neutral) mDeath)
    (fromMaybe (attackBump neutral) mAttackBump)
    (fromMaybe (hitPointsChange neutral) mHitPointsChange)
    (fromMaybe (scoreChange neutral) mScoreChange)
  where
    neutral :: AttackEffect = mempty

instance Semigroup AttackEffect where
  AttackEffect {death = d1, attackBump = ab1, hitPointsChange = hp1, scoreChange = c1}
    <> AttackEffect {death = d2, attackBump = ab2, hitPointsChange = hp2, scoreChange = c2} =
      AttackEffect
        { death = d1 || d2,
          attackBump = ab1 || ab2,
          hitPointsChange = hp1 + hp2,
          scoreChange = c1 + c2
        }

instance Monoid AttackEffect where
  mempty =
    AttackEffect
      { death = False,
        attackBump = False,
        hitPointsChange = 0,
        scoreChange = 0
      }

newtype AttackEffects = AttackEffects {unAttackEffects :: Map.Map CardSpot AttackEffect}
  deriving (Eq, Generic, Show)

instance Semigroup AttackEffects where
  AttackEffects m1 <> AttackEffects m2 = AttackEffects (Map.unionWith (<>) m1 m2)

instance Monoid AttackEffects where
  mempty = AttackEffects mempty

type family InPlaceType (p :: Phase) where
  InPlaceType Core = CardsOnTable
  InPlaceType UI = AttackEffects

type family HandElemType (p :: Phase) where
  HandElemType Core = Card Core
  HandElemType UI = Int

type family InHandType (p :: Phase) where
  InHandType p = [HandElemType p]

type family ScoreType (p :: Phase) where
  ScoreType Core = Int
  ScoreType UI = ()

type family StackType (p :: Phase) where
  StackType Core = [CardIdentifier]
  StackType UI = Int -- Discarded->Stack transfer

type family DiscardedType (p :: Phase) where
  DiscardedType Core = [CardIdentifier]
  DiscardedType UI = Int -- Discarded->Stack transfer

type Forall (c :: Type -> Constraint) (p :: Phase) =
  ( c (HandElemType p),
    c (InPlaceType p),
    c (InHandType p),
    c (ScoreType p),
    c (StackType p),
    c (DiscardedType p)
  )

data PlayerPart (p :: Phase) = PlayerPart
  { -- | Cards on the board
    inPlace :: InPlaceType p,
    -- | Cards in hand
    inHand :: InHandType p,
    -- | The score of this player
    score :: ScoreType p,
    stack :: StackType p,
    discarded :: DiscardedType p
  }
  deriving (Generic)

deriving instance Board.Forall Eq p => Eq (PlayerPart p)

deriving instance Board.Forall Show p => Show (PlayerPart p)

instance Semigroup (PlayerPart UI) where
  PlayerPart inPlace1 inHand1 () s1 d1 <> PlayerPart inPlace2 inHand2 () s2 d2 =
    PlayerPart (inPlace1 <> inPlace2) (inHand1 <> inHand2) () (s1 + s2) (d1 + d2)

instance Monoid (PlayerPart UI) where
  mempty = PlayerPart mempty mempty mempty 0 0

newtype HandIndex = HandIndex {unHandIndex :: Int}
  deriving (Eq, Show, Generic, Enum)

lookupHand ::
  MonadError Text m =>
  [a] ->
  Int ->
  m a
lookupHand hand i
  | i < 0 = throwError $ sformat ("Invalid hand index: " % hex) i
  | i >= handLength =
    throwError $
      sformat
        ("Invalid hand index: " % hex % ". Hand has " % hex % " card(s).")
        i
        handLength
  | otherwise = return $ hand !! i
  where
    handLength = length hand

data PlayerSpot = PlayerBottom | PlayerTop
  deriving (Enum, Eq, Ord, Show, Generic)

allPlayersSpots :: [PlayerSpot]
allPlayersSpots = [PlayerBottom ..]

startingPlayerSpot :: PlayerSpot
startingPlayerSpot = PlayerBottom

endingPlayerSpot :: PlayerSpot
endingPlayerSpot = PlayerTop

data Board (p :: Phase) = Board
  { playerTop :: PlayerPart p,
    playerBottom :: PlayerPart p
  }
  deriving (Generic)

deriving instance Board.Forall Eq p => Eq (Board p)

deriving instance Board.Forall Show p => Show (Board p)

instance Semigroup (Board UI) where
  Board top1 bot1 <> Board top2 bot2 =
    Board (top1 <> top2) (bot1 <> bot2)

instance Monoid (Board UI) where
  mempty = Board mempty mempty

boardAddToHand :: Board p -> PlayerSpot -> HandElemType p -> Board p
boardAddToHand board pSpot handElem =
  boardSetPart board pSpot $ part {inHand = hand'}
  where
    part = boardToPart board pSpot
    hand = inHand part
    hand' = snoc hand handElem

boardSetCreature :: Board Core -> PlayerSpot -> CardSpot -> Creature Core -> Board Core
boardSetCreature board pSpot cSpot creature =
  boardSetPart board pSpot part'
  where
    part = boardToPart board pSpot
    part' = part & #inPlace . at cSpot ?~ creature

boardSetDiscarded :: Board p -> PlayerSpot -> DiscardedType p -> Board p
boardSetDiscarded board pSpot discarded =
  boardSetPart board pSpot $ part {discarded = discarded}
  where
    part = boardToPart board pSpot

boardSetHand :: Board p -> PlayerSpot -> InHandType p -> Board p
boardSetHand board pSpot hand =
  boardSetPart board pSpot $ part {inHand = hand}
  where
    part = boardToPart board pSpot

boardSetPart :: Board p -> PlayerSpot -> PlayerPart p -> Board p
boardSetPart board PlayerTop part = board {playerTop = part}
boardSetPart board PlayerBottom part = board {playerBottom = part}

boardSetStack :: Board p -> PlayerSpot -> StackType p -> Board p
boardSetStack board pSpot stack =
  boardSetPart board pSpot $ part {stack = stack}
  where
    part = boardToPart board pSpot

boardToHoleyInPlace :: Board Core -> [(PlayerSpot, CardSpot, Maybe (Creature Core))]
boardToHoleyInPlace board =
  [ (pSpot, cSpot, maybeCreature)
    | pSpot <- allPlayersSpots,
      cSpot <- allCardsSpots,
      let maybeCreature = boardToInPlaceCreature board (spotToLens pSpot) cSpot
  ]

boardToInHandCreaturesToDraw :: Board Core -> Lens' (Board Core) (PlayerPart Core) -> [Creature Core]
boardToInHandCreaturesToDraw board player =
  board ^.. player . #inHand . folded . #_CreatureCard

boardToDiscarded :: Board p -> PlayerSpot -> DiscardedType p
boardToDiscarded Board {playerTop} PlayerTop = discarded playerTop
boardToDiscarded Board {playerBottom} PlayerBottom = discarded playerBottom

boardToHand :: Board p -> PlayerSpot -> InHandType p
boardToHand Board {playerTop} PlayerTop = inHand playerTop
boardToHand Board {playerBottom} PlayerBottom = inHand playerBottom

boardToPart :: Board p -> PlayerSpot -> PlayerPart p
boardToPart Board {playerTop} PlayerTop = playerTop
boardToPart Board {playerBottom} PlayerBottom = playerBottom

boardToStack :: Board p -> PlayerSpot -> StackType p
boardToStack Board {playerTop} PlayerTop = stack playerTop
boardToStack Board {playerBottom} PlayerBottom = stack playerBottom

boardToInPlaceCreature ::
  Board Core ->
  Lens' (Board Core) (PlayerPart Core) ->
  CardSpot ->
  Maybe (Creature Core)
boardToInPlaceCreature board player cSpot =
  board ^. player . #inPlace . at cSpot

emptyBoard :: Board Core
emptyBoard =
  Board {playerTop = emptyPlayerPart, playerBottom = emptyPlayerPart}

emptyPlayerPart :: PlayerPart Core
emptyPlayerPart = PlayerPart {..}
  where
    inPlace = Map.empty
    inHand = []
    score = 0
    stack = []
    discarded = []

exampleBoard :: [Card UI] -> Board Core
exampleBoard cards =
  Board topPlayer botPlayer
  where
    humanArcher = unsafeCreatureWithID cards (CreatureID Archer Human)
    humanGeneral = unsafeCreatureWithID cards (CreatureID General Human)
    humanSpearman = unsafeCreatureWithID cards (CreatureID Spearman Human)
    undeadArcher = unsafeCreatureWithID cards (CreatureID Archer Undead)
    undeadMummy = unsafeCreatureWithID cards (CreatureID Mummy Undead)
    undeadVampire = unsafeCreatureWithID cards (CreatureID Vampire Undead)
    topCards :: CardsOnTable =
      Map.fromList
        [ (TopLeft, undeadArcher),
          (Bottom, undeadVampire),
          (BottomRight, undeadMummy)
        ]
    (topHand, topStack) = splitAt handSize $ initialDeck cards Undead
    topStack' = map cardToIdentifier topStack
    topPlayer = PlayerPart topCards topHand 0 topStack' []
    botCards :: CardsOnTable =
      makeBottomCardsOnTable $
        Map.fromList
          [ (TopLeft, humanArcher),
            (Top, humanSpearman),
            (TopRight, humanGeneral),
            (BottomLeft, humanArcher)
          ]
    (botHand, botStack) = splitAt handSize $ initialDeck cards Human
    botStack' = map cardToIdentifier botStack
    botPlayer = PlayerPart botCards botHand 0 botStack' []

-- Whether a spot is in the back line
inTheBack :: CardSpot -> Bool
inTheBack TopLeft = True
inTheBack Top = True
inTheBack TopRight = True
inTheBack _ = False

-- | The other spot
otherPlayerSpot :: PlayerSpot -> PlayerSpot
otherPlayerSpot PlayerBottom = PlayerTop
otherPlayerSpot PlayerTop = PlayerBottom

spotToLens :: PlayerSpot -> Lens' (Board p) (PlayerPart p)
spotToLens =
  \case
    PlayerBottom -> #playerBottom
    PlayerTop -> #playerTop
