{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Handling of arrows and WASD in 'WorldView'. Module is meant to be  used
-- qualified.
module Direction where

import GHC.Generics (Generic)
import Miso.Subscription (Arrows (..))
import Nat (Nat)

newtype Coord = Coord (Nat, Nat)
  deriving (Eq, Generic, Show)

-- | Direction of arrows
data T = DirDown | DirLeft | DirRight | DirUp

instance Show T where
  show = \case DirDown -> "Down"; DirLeft -> "Left"; DirRight -> "Right"; DirUp -> "Up"

-- | Shifts a coordinate by a direction, returning the new coordinate
move :: T -> Coord -> Maybe Coord
move dir (Coord (x, y)) =
  Coord <$> case dir of
    DirDown -> Just (x, y + 1)
    DirLeft | x > 0 -> Just (x - 1, y)
    DirRight -> Just (x + 1, y)
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