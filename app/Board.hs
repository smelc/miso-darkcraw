{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Board
  ( allCardsSpots,
    boardToCardsInHand,
    boardToCardsInPlace,
    Board,
    CardSpot (..),
    exampleBoard,
    PlayerSpot (..),
  )
where

import Card
import Data.Bifunctor
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set

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
  deriving (Enum, Eq, Ord, Show)

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

data PlayerPart
  = PlayerPart
      { -- | Cards on the board
        inPlace :: CardsOnTable,
        -- | Cards in hand
        inHand :: [Card Core]
      }
  deriving (Eq)

data PlayerSpot = PlayerBottom | PlayerTop
  deriving (Enum, Eq, Ord, Show)

allPlayersSpots :: [PlayerSpot]
allPlayersSpots = [PlayerBottom ..]

type Board = Map.Map PlayerSpot PlayerPart

boardToCardsInPlace :: Board -> [(PlayerSpot, CardSpot, Creature Core)]
boardToCardsInPlace board =
  [ (pspot, cspot, creature)
    | (pspot, PlayerPart {inPlace}) <- Map.toList board,
      (cspot, creature) <- Map.toList inPlace
  ]

boardToCardsInHand :: Board -> [(PlayerSpot, Card Core)]
boardToCardsInHand board =
  [ (pspot, card)
    | (pspot, PlayerPart {inHand}) <- Map.toList board,
      card <- inHand
  ]

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
    botHand = [CreatureCard hArcher, CreatureCard hArcher]
    botCards :: CardsOnTable =
      makeBottomCardsOnTable $
        Map.fromList
          [ (Top, hGeneral),
            (TopLeft, hSpearman)
          ]
    botPlayer = PlayerPart botCards botHand
