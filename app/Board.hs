{-# LANGUAGE DataKinds #-}
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

-- | A convenience method for building an instance of CardsOnTable
-- | from a list. First member of the list if TopLeft, then Top, then
-- | TopRight, then BottomLeft, then Bottom, then BottomRight. Maybe
-- | allow to skip a spot. Items after the sixth one are simply ignored.
listToCardsOnTable :: [Maybe (Creature Core)] -> CardsOnTable
listToCardsOnTable maybeCreatures =
  impl (take (length allCardsSpots) maybeCreatures) 0 Map.empty
  where
    impl :: [Maybe (Creature Core)] -> Int -> CardsOnTable -> CardsOnTable
    impl [] idx acc = acc
    impl (fst : tail) idx acc =
      let nextAcc =
            case fst of
              Nothing -> acc
              Just creature -> Map.insert (allCardsSpots !! idx) creature acc
       in impl tail (idx + 1) nextAcc

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

_listProduct :: [(a, [b])] -> [[(a, b)]]
_listProduct [] = []
_listProduct ((a, bs) : rest) =
  [(a, b) | b <- bs] : _listProduct rest

boardToCardsInPlace :: Board -> [(PlayerSpot, CardSpot, Creature Core)]
boardToCardsInPlace board =
  [(a, b, c) | (a, (b, c)) <- board''']
  where
    board' :: [(PlayerSpot, PlayerPart)] = Map.toList board
    sndFiddler :: PlayerPart -> [(CardSpot, Creature Core)] =
      Map.toList . inPlace
    board'' :: [(PlayerSpot, [(CardSpot, Creature Core)])] =
      Prelude.map (Data.Bifunctor.second sndFiddler) board'
    board''' = concat $ _listProduct board''

boardToCardsInHand :: Board -> [(PlayerSpot, Card Core)]
boardToCardsInHand board =
  concat $ _listProduct board''
  where
    board' :: [(PlayerSpot, PlayerPart)] = Map.toList board
    sndFiddler :: PlayerPart -> [(CardSpot, Creature Core)] =
      Map.toList . inPlace
    board'' :: [(PlayerSpot, [Card Core])] =
      Prelude.map (Data.Bifunctor.second inHand) board'

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
      listToCardsOnTable
        [ Just udArcher,
          Nothing,
          Nothing,
          Nothing,
          Just udVampire,
          Just udMummy
        ]
    topPlayer = PlayerPart topCards []

    botHand = [CreatureCard hArcher, CreatureCard hArcher]
    botCards :: CardsOnTable =
      listToCardsOnTable
        [ Nothing,
          Nothing,
          Nothing,
          Nothing,
          Just hGeneral,
          Just hSpearman
        ]
    botPlayer = PlayerPart botCards botHand
