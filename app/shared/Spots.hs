{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module defining the player spots as well as the card spots.
-- Historically this was in 'Board' but was deported to a new file
-- to be visible both from 'Board' and 'Command'.
-- |
module Spots where

import qualified Data.List.NonEmpty as NE
import GHC.Generics

-- | The spot of a card, as visible from the top of the screen. For the
-- | bottom part, think as if it was in the top, turning the board
-- | 180 degrees clockwise; or use these values and map [bottomSpotOfTopVisual].
data Card
  = TopLeft
  | Top
  | TopRight
  | BottomLeft
  | Bottom
  | BottomRight
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

-- | The position of a player
data Player = PlayerBot | PlayerTop
  deriving (Bounded, Enum, Eq, Ord, Generic)

instance Show Spots.Player where
  show PlayerBot = "bot"
  show PlayerTop = "top"

-- | The various kinds of neighbors
data Neighborhood
  = -- | Neighbors to the left and the right
    Cardinal
  | -- | Neighbors in diagonals
    Diagonal
  | -- | Cardinal + diagnoal neighbors
    All
  deriving (Eq, Generic, Show)

-- * Functions about 'CardSpot'

-- | All 'Spots.Card' values.
allCards :: [Spots.Card]
allCards = [minBound ..]

-- | All 'Spots.Card' values, as a non-empty list.
allCardsNE :: NE.NonEmpty Spots.Card
allCardsNE = minBound NE.:| [succ minBound ..]

-- | Returns a bottom position, by taking a position that makes sense visually
-- I.e. if you give this method [TopLeft], it'll correspond to the [TopLeft]
-- bottom position that you SEE; even if positions make sense for the top
-- part. This method takes care of translating correctly.
bottomSpotOfTopVisual :: Spots.Card -> Spots.Card
bottomSpotOfTopVisual = \case
  TopLeft -> BottomRight
  Top -> Bottom
  TopRight -> BottomLeft
  BottomLeft -> TopRight
  Bottom -> Top
  BottomRight -> TopLeft

botSpots :: [Spots.Card]
botSpots = filter (not . inTheBack) allCards

topSpots :: [Spots.Card]
topSpots = filter inTheBack allCards

-- | Whether a spot is in the back line
inTheBack :: Spots.Card -> Bool
inTheBack TopLeft = True
inTheBack Top = True
inTheBack TopRight = True
inTheBack _ = False

-- | Whether a spot is in the front line
inFront :: Spots.Card -> Bool
inFront = not . inTheBack

neighbors :: Neighborhood -> Spots.Card -> [Spots.Card]
neighbors All pSpot = neighbors Diagonal pSpot ++ neighbors Cardinal pSpot
neighbors Cardinal pSpot =
  case pSpot of
    TopLeft -> [Top, BottomLeft]
    Top -> [TopLeft, TopRight, Bottom]
    TopRight -> [Top, BottomRight]
    BottomLeft -> [TopLeft, Bottom]
    Bottom -> [BottomLeft, Top, BottomRight]
    BottomRight -> [Bottom, TopRight]
neighbors Diagonal pSpot =
  case pSpot of
    TopLeft -> [Bottom]
    Top -> [BottomLeft, BottomRight]
    TopRight -> [Bottom]
    BottomLeft -> [Top]
    Bottom -> [TopLeft, TopRight]
    BottomRight -> [Top]

-- | Given a frontline spot, the corresponding backline spot. Given
-- a backline spot, the corresponding frontline spot.
switchLine :: Spots.Card -> Spots.Card
switchLine TopLeft = BottomLeft
switchLine Top = Bottom
switchLine TopRight = BottomRight
switchLine BottomLeft = TopLeft
switchLine Bottom = Top
switchLine BottomRight = TopRight

-- | All spots in the same line
line :: Spots.Card -> [Spots.Card]
line = \case
  TopLeft -> top
  Top -> top
  TopRight -> top
  BottomLeft -> bot
  Bottom -> bot
  BottomRight -> bot
  where
    top = [TopLeft, Top, TopRight]
    bot = [BottomLeft, Bottom, BottomRight]

-- * Functions about 'PlayerSpot'

-- | All 'Spots.Player' values. TODO @smelc rename me into allPlayers
allPlayersSpots :: [Spots.Player]
allPlayersSpots = [minBound ..]

-- | The spot of the player that starts a game
startingPlayerSpot :: Spots.Player
startingPlayerSpot = PlayerBot

-- | The spot of the player that ends a game
endingPlayerSpot :: Spots.Player
endingPlayerSpot = PlayerTop

-- | The other spot
-- XXX @smelc rename me to other and use me qualified
otherPlayerSpot :: Spots.Player -> Spots.Player
otherPlayerSpot PlayerBot = PlayerTop
otherPlayerSpot PlayerTop = PlayerBot
