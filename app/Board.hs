{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Board
  ( allCardsSpots,
    boardToCardsInPlace,
    boardToInHandCreaturesToDraw,
    boardToHand,
    Board(..),
    CardSpot (..),
    exampleBoard,
    playingPlayerSpot,
    playingPlayerPart,
    PlayerPart (..),
    PlayerSpot (..),
  )
where

import Card
import Control.Lens
import Data.Generics.Labels
import qualified Data.Map.Strict as Map
import Data.Maybe
import GHC.Generics (Generic)

-- | The spot of a card, as visible from the top of the screen. For the
-- | bottom part, think as if it was in the top, turning the board
-- | 180 degrees clockwise
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
  Map.mapKeys translate
  where
    translate = \case
      TopLeft -> BottomRight
      Top -> Bottom
      TopRight -> BottomLeft
      BottomLeft -> TopRight
      Bottom -> Top
      BottomRight -> TopLeft

data PlayerPart = PlayerPart
  { -- | Cards on the board
    inPlace :: CardsOnTable,
    -- | Cards in hand
    inHand :: [Card Core]
  }
  deriving (Eq, Generic)

data PlayerSpot = PlayerBottom | PlayerTop
  deriving (Enum, Eq, Ord, Show, Generic)

allPlayersSpots :: [PlayerSpot]
allPlayersSpots = [PlayerBottom ..]

data Board = Board
  { playerTop :: PlayerPart,
    playerBottom :: PlayerPart
  } deriving (Eq, Generic)

boardToList :: Board -> [(PlayerSpot, PlayerPart)]
boardToList Board {playerTop, playerBottom} =
  [(PlayerTop, playerTop), (PlayerBottom, playerBottom)]

boardToCardsInPlace :: Board -> [(PlayerSpot, CardSpot, Creature Core)]
boardToCardsInPlace board =
  [ (pspot, cspot, creature)
    | (pspot, PlayerPart {inPlace}) <- boardToList board,
      (cspot, creature) <- Map.toList inPlace
  ]

boardToInHandCreaturesToDraw :: Board -> [Creature Core]
boardToInHandCreaturesToDraw board =
  board ^.. playingPlayerPart . #inHand . folded . #_CreatureCard

boardToHand :: Board -> Lens' Board PlayerPart -> [Card Core]
boardToHand board player =
  board ^. player . #inHand

exampleBoard :: [Card UI] -> Board
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

playingPlayerPart :: Lens' Board PlayerPart
playingPlayerPart = #playerBottom