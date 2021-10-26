{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module defining the player spots as well as the card spots.
-- Historically this was in 'Board' but was deported to a new file
-- to be visible both from 'Board' and 'Command'.
-- |
module Spots where

import GHC.Generics

-- | The spot of a card, as visible from the top of the screen. For the
-- | bottom part, think as if it was in the top, turning the board
-- | 180 degrees clockwise; or use these values and map [bottomSpotOfTopVisual].
-- TODO @smelc rename me to Card and use me qualified
data CardSpot
  = TopLeft
  | Top
  | TopRight
  | BottomLeft
  | Bottom
  | BottomRight
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

-- | The position of a player
-- TODO @smelc rename me to Player and use me qualified
data PlayerSpot = PlayerBot | PlayerTop
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

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

allCardsSpots :: [CardSpot]
allCardsSpots = [minBound ..]

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

botSpots :: [CardSpot]
botSpots = filter (not . inTheBack) allCardsSpots

topSpots :: [CardSpot]
topSpots = filter inTheBack allCardsSpots

-- | Whether a spot is in the back line
inTheBack :: CardSpot -> Bool
inTheBack TopLeft = True
inTheBack Top = True
inTheBack TopRight = True
inTheBack _ = False

-- | Whether a spot is in the front line
inFront :: CardSpot -> Bool
inFront = not . inTheBack

neighbors :: Neighborhood -> CardSpot -> [CardSpot]
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
switchLine :: CardSpot -> CardSpot
switchLine TopLeft = BottomLeft
switchLine Top = Bottom
switchLine TopRight = BottomRight
switchLine BottomLeft = TopLeft
switchLine Bottom = Top
switchLine BottomRight = TopRight

-- | All spots in the same line
line :: CardSpot -> [CardSpot]
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

allPlayersSpots :: [PlayerSpot]
allPlayersSpots = [minBound ..]

-- | The spot of the player that starts a game
startingPlayerSpot :: PlayerSpot
startingPlayerSpot = PlayerBot

-- | The spot of the player that ends a game
endingPlayerSpot :: PlayerSpot
endingPlayerSpot = PlayerTop

-- | The other spot
otherPlayerSpot :: PlayerSpot -> PlayerSpot
otherPlayerSpot PlayerBot = PlayerTop
otherPlayerSpot PlayerTop = PlayerBot
