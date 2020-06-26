{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
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
    boardToCardsInPlace,
    boardToInHandCreaturesToDraw,
    boardToInPlaceCreature,
    boardToHand,
    Board (..),
    CardSpot (..),
    exampleBoard,
    otherPlayerSpot,
    playingPlayerSpot,
    playingPlayerPart,
    PlayerPart (..),
    PlayerSpot (..),
    spotToLens,
  )
where

import Card
import Control.Lens
import Data.Generics.Labels
import Data.Kind (Constraint, Type)
import qualified Data.Map.Strict as Map
import Data.Maybe
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

data AttackEffect
  = -- | Death (hits points change that makes hit points go 0 or negative)
    Death
  | -- | Hit points change
    HitPointsChange Int
  deriving (Generic)

instance Semigroup AttackEffect where
  Death <> _ = Death
  _ <> Death = Death
  HitPointsChange i <> HitPointsChange j = HitPointsChange (i + j)

newtype AttackEffects = AttackEffects (Map.Map CardSpot AttackEffect)
  deriving (Generic)

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

type Forall (c :: Type -> Constraint) (p :: Phase) =
  ( c (InPlaceType p),
    c (InHandType p)
  )

data PlayerPart (p :: Phase) = PlayerPart
  { -- | Cards on the board
    inPlace :: InPlaceType p,
    -- | Cards in hand
    inHand :: InHandType p
  }
  deriving (Generic)

deriving instance Board.Forall Eq p => Eq (PlayerPart p)

data PlayerSpot = PlayerBottom | PlayerTop
  deriving (Enum, Eq, Ord, Show, Generic)

allPlayersSpots :: [PlayerSpot]
allPlayersSpots = [PlayerBottom ..]

data Board (p :: Phase) = Board
  { playerTop :: PlayerPart p,
    playerBottom :: PlayerPart p
  }
  deriving (Generic)

deriving instance Board.Forall Eq p => Eq (Board p)

boardToList :: Board p -> [(PlayerSpot, PlayerPart p)]
boardToList Board {playerTop, playerBottom} =
  [(PlayerTop, playerTop), (PlayerBottom, playerBottom)]

boardToCardsInPlace :: Board Core -> [(PlayerSpot, CardSpot, Creature Core)]
boardToCardsInPlace board =
  [ (pspot, cspot, creature)
    | (pspot, PlayerPart {inPlace}) <- boardToList board,
      (cspot, creature) <- Map.toList inPlace
  ]

boardToInHandCreaturesToDraw :: Board Core -> [Creature Core]
boardToInHandCreaturesToDraw board =
  board ^.. playingPlayerPart . #inHand . folded . #_CreatureCard

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

exampleBoard :: [Card UI] -> Board Core
exampleBoard cards =
  Board topPlayer botPlayer
  where
    creatures :: [Creature Core] =
      cards ^.. folded . #_CreatureCard . to creatureUI2CreatureCore
    getCardByID searched =
      head $ filter (\c -> creatureId c == searched) creatures
    humanArcher = getCardByID (CreatureID Archer Human)
    humanGeneral = getCardByID (CreatureID General Human)
    humanSpearman = getCardByID (CreatureID Spearman Human)
    undeadArcher = getCardByID (CreatureID Archer Undead)
    undeadMummy = getCardByID (CreatureID Mummy Undead)
    undeadVampire = getCardByID (CreatureID Vampire Undead)
    topCards :: CardsOnTable =
      Map.fromList
        [ (TopLeft, undeadArcher),
          (Bottom, undeadVampire),
          (BottomRight, undeadMummy)
        ]
    topPlayer = PlayerPart topCards []
    botHand = [CreatureCard humanArcher, CreatureCard humanSpearman]
    botCards :: CardsOnTable =
      makeBottomCardsOnTable $
        Map.fromList
          [ (Top, humanGeneral),
            (TopLeft, humanSpearman)
          ]
    botPlayer = PlayerPart botCards botHand

playingPlayerSpot :: PlayerSpot
playingPlayerSpot = PlayerBottom

playingPlayerPart :: Lens' (Board Core) (PlayerPart Core)
playingPlayerPart = #playerBottom

-- | The other spot
otherPlayerSpot :: PlayerSpot -> PlayerSpot
otherPlayerSpot PlayerBottom = PlayerTop
otherPlayerSpot PlayerTop = PlayerBottom

spotToLens :: PlayerSpot -> Lens' (Board Core) (PlayerPart Core)
spotToLens =
  \case
    PlayerBottom -> #playerBottom
    PlayerTop -> #playerTop
