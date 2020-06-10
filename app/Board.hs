{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Board
  ( allCardsSpots,
    boardHand,
    boardToCardsInPlace,
    boardToInHandCreaturesToDraw,
    Board,
    CardSpot (..),
    exampleBoard,
    playingPlayerSpot,
    PlayerPart (..),
    PlayerSpot (..),
  )
where

import Card
import Control.Lens
import Data.Function ((&))
import Data.Generics.Labels
import qualified Data.Map.Strict as Map
import Data.Maybe
import GHC.Generics

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

-- FIXME smelc Use a record with two fields, as proposed by @polux
-- to avoid dealing with Map.lookup returning Maybe
type Board = Map.Map PlayerSpot PlayerPart

boardToCardsInPlace :: Board -> [(PlayerSpot, CardSpot, Creature Core)]
boardToCardsInPlace board =
  [ (pspot, cspot, creature)
    | (pspot, PlayerPart {inPlace}) <- Map.toList board,
      (cspot, creature) <- Map.toList inPlace
  ]

boardToInHandCreaturesToDraw :: Board -> [Creature Core]
boardToInHandCreaturesToDraw board =
  board ^.. ix playingPlayerSpot . #inHand . folded . #_CreatureCard

boardHand :: Board -> PlayerSpot -> [Card Core]
boardHand board pSpot = board ^. ix pSpot . #inHand

exampleBoard :: [Card UI] -> Board
exampleBoard cards =
  Map.fromList [(PlayerBottom, botPlayer), (PlayerTop, topPlayer)]
  where
    humanArcher = CreatureID Archer Human
    humanGeneral = CreatureID General Human
    humanSpearman = CreatureID Spearman Human
    undeadArcher = CreatureID Archer Undead
    undeadMummy = CreatureID Mummy Undead
    undeadVampire = CreatureID Vampire Undead
    creatures :: [Creature Core] =
      map creatureUI2CreatureCore $ mapMaybe card2Creature cards
    getCardByID searched =
      head $ filter (\c -> creatureId c == searched) creatures
    hArcher = getCardByID humanArcher
    hGeneral = getCardByID humanGeneral
    hSpearman = getCardByID humanSpearman
    udArcher = getCardByID undeadArcher
    udMummy = getCardByID undeadMummy
    udVampire = getCardByID undeadVampire
    topCards :: CardsOnTable =
      Map.fromList
        [ (TopLeft, udArcher),
          (Bottom, udVampire),
          (BottomRight, udMummy)
        ]
    topPlayer = PlayerPart topCards []
    botHand = [CreatureCard hArcher, CreatureCard hSpearman]
    botCards :: CardsOnTable =
      makeBottomCardsOnTable $
        Map.fromList
          [ (Top, hGeneral),
            (TopLeft, hSpearman)
          ]
    botPlayer = PlayerPart botCards botHand

playingPlayerSpot :: PlayerSpot
playingPlayerSpot = PlayerBottom
