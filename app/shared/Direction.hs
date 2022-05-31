{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Handling of arrows and WASD in 'WorldView'. Module is meant to be  used
-- qualified.
module Direction where

import Data.Maybe
import GHC.Generics (Generic)
import Miso.Subscription (Arrows (..))
import Nat (Nat)

newtype Coord = Coord (Nat, Nat)
  deriving (Eq, Generic, Ord, Show)

topLeftOf :: Coord -> Coord -> Bool
topLeftOf (Coord (x1, y1)) (Coord (x2, y2)) = x1 Prelude.<= x2 && y1 Prelude.<= y2

(+) :: Direction.Coord -> Direction.Coord -> Direction.Coord
(+) (Coord (x1, y1)) (Coord (x2, y2)) = Coord (x1 Prelude.+ x2, y1 Prelude.+ y2)

-- | Removes the 'Coord' constructor
unCoord :: Coord -> (Nat, Nat)
unCoord (Coord (x, y)) = (x, y)

-- | Maps over the content of a 'Coord'
mapCoord :: (Nat -> Nat) -> Coord -> Coord
mapCoord f (Coord (x, y)) = Coord (f x, f y)

-- | Substracts two coords
minus :: Coord -> Coord -> Coord
minus (Coord (x1, y1)) (Coord (x2, y2)) = Coord (x1 - x2, y1 - y2)

-- | Direction of arrows
data T = DirDown | DirLeft | DirRight | DirUp
  deriving (Bounded, Enum)

-- | All possible directions
all :: [Direction.T]
all = [minBound ..]

instance Show T where
  show = \case DirDown -> "Down"; DirLeft -> "Left"; DirRight -> "Right"; DirUp -> "Up"

-- | Shifts a coordinate by a direction, returning the new coordinate
move :: T -> Coord -> Maybe Coord
move dir (Coord (x, y)) =
  Coord <$> case dir of
    DirDown -> Just (x, y Prelude.+ 1)
    DirLeft | x > 0 -> Just (x - 1, y)
    DirRight -> Just (x Prelude.+ 1, y)
    DirUp | y > 0 -> Just (x, y - 1)
    _ -> Nothing

-- | From miso 'Arrows' to our direction type 'T'
ofArrows :: Arrows -> Maybe T
ofArrows Arrows {arrowX, arrowY} =
  case (arrowX, arrowY) of
    (0, 0) -> Nothing
    (-1, 0) -> Just DirLeft
    (1, 0) -> Just DirRight
    (0, 1) -> Just DirUp
    (0, -1) -> Just DirDown
    _ -> Nothing -- We ignore double key pressed like up and left for example

-- | Whether 'c1' and 'c2' are adjacent.
isAdjacent :: Coord -> Coord -> Bool
isAdjacent c1 c2 = c2 `elem` catMaybes [move dir c1 | dir <- Direction.all]
