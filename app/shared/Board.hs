{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
    boardToCardsInPlace,
    boardToInHandCreaturesToDraw,
    boardToInPlaceCreature,
    boardToHand,
    Board (..),
    CardSpot (..),
    createAttackEffect,
    endingPlayerSpot,
    emptyInPlaceBoard,
    exampleBoard,
    HandIndex (..),
    inTheBack,
    lookupHand,
    otherPlayerSpot,
    PlayerPart (..),
    PlayerSpot (..),
    startingPlayerSpot,
    spotToLens,
  )
where

import Card
import Constants (handSize)
import Control.Lens
import Control.Monad.Except (MonadError, throwError)
import Data.Generics.Labels
import Data.Kind (Constraint, Type)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import Formatting ((%), format, hex, sformat)
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

type family InHandType (p :: Phase) where
  InHandType Core = [Card Core]
  InHandType UI = ()

type family ScoreType (p :: Phase) where
  ScoreType Core = Int
  ScoreType UI = ()

type family StackType (p :: Phase) where
  StackType Core = [Card Core]
  StackType UI = ()

type family DiscardedType (p :: Phase) where
  DiscardedType Core = [Card Core]
  DiscardedType UI = ()

type Forall (c :: Type -> Constraint) (p :: Phase) =
  ( c (InPlaceType p),
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
  PlayerPart inPlace1 () () () () <> PlayerPart inPlace2 () () () () =
    PlayerPart (inPlace1 <> inPlace2) () () () ()

instance Monoid (PlayerPart UI) where
  mempty = PlayerPart mempty mempty mempty mempty mempty

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

boardSetPart :: Board p -> PlayerSpot -> PlayerPart p -> Board p
boardSetPart board PlayerTop part = board {playerTop = part}
boardSetPart board PlayerBottom part = board {playerBottom = part}

boardToCardsInPlace :: Board Core -> [(PlayerSpot, CardSpot, Maybe (Creature Core))]
boardToCardsInPlace board =
  [ (pSpot, cSpot, maybeCreature)
    | pSpot <- allPlayersSpots,
      cSpot <- allCardsSpots,
      let maybeCreature = boardToInPlaceCreature board (spotToLens pSpot) cSpot
  ]

boardToInHandCreaturesToDraw :: Board Core -> Lens' (Board Core) (PlayerPart Core) -> [Creature Core]
boardToInHandCreaturesToDraw board player =
  board ^.. player . #inHand . folded . #_CreatureCard

boardToHand :: Board Core -> Lens' (Board Core) (PlayerPart Core) -> [Card Core]
boardToHand board player =
  board ^. player . #inHand

boardToInPlaceCreature ::
  Board Core ->
  Lens' (Board Core) (PlayerPart Core) ->
  CardSpot ->
  Maybe (Creature Core)
boardToInPlaceCreature board player cSpot =
  board ^. player . #inPlace . at cSpot

-- | A board with nothing in place, and solely the following
-- | hand for the top player. For testing.
emptyInPlaceBoard :: [Card UI] -> InHandType Core -> Board Core
emptyInPlaceBoard cards topHand =
  Board topPlayer botPlayer
  where
    (topCards, topStack) = (Map.empty, [])
    topPlayer = PlayerPart topCards topHand 0 topStack []
    botPlayer = PlayerPart Map.empty [] 0 [] []

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
    topPlayer = PlayerPart topCards topHand 0 topStack []
    botCards :: CardsOnTable =
      makeBottomCardsOnTable $
        Map.fromList
          [ (TopLeft, humanArcher),
            (Top, humanSpearman),
            (TopRight, humanGeneral),
            (BottomLeft, humanArcher)
          ]
    (botHand, botStack) = splitAt handSize $ initialDeck cards Human
    botPlayer = PlayerPart botCards botHand 0 botStack []

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
