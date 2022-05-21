{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Module displaying the world map, which is the first view
-- in legendary edition as well as the view between levels.
module WorldView where

import qualified Constants
import qualified Data.Bifunctor as Bifunctor
import Data.Function ((&))
import qualified Direction
import Miso
import qualified Miso.String as MisoString
import qualified Model
import qualified Nat
import qualified Network
import qualified Roads
import qualified Shared
import qualified Tile
import qualified Update
import ViewInternal (Position (..), Styled, px)
import qualified ViewInternal

viewWorldModel :: Model.World -> Styled (View Update.Action)
viewWorldModel Model.World {position, shared, topLeft} = do
  let man = manView zpp
  return $
    div_
      [style_ bgStyle]
      [man]
  where
    (z, zpp) = (0, z + 1)
    bgStyle =
      ViewInternal.zpltwh z Relative 0 0 pxWidth pxHeight
        <> "background-image" =: Constants.assetsUrl "world.png"
        <> "background-position-x" =: px (-(Constants.cps * 13))
        <> "background-position-y" =: px pxHeight
    (pxHeight, pxWidth) = (cellsHeight * Constants.cps, cellsWidth * Constants.cps)
    manView z =
      div_
        [ style_ $
            ViewInternal.zpltwh
              z
              ViewInternal.Absolute
              (Nat.natToInt manX)
              (Nat.natToInt manY)
              manTilePx
              manTilePx
        ]
        [ViewInternal.imgCellwh manFilepath Constants.cps Constants.cps Nothing]
      where
        Direction.Coord (manX, manY) =
          position `Direction.minus` topLeft
            & Direction.mapCoord (\x -> x * (Nat.intToNat Constants.cps))
    (manTileSize, manTilePx) = (Tile.TwentyFour, Tile.sizeToNat manTileSize & Nat.natToInt)
    manFilepath =
      Shared.tileToFilepath shared Tile.Man manTileSize
        & Tile.filepathToString
        & MisoString.ms

-- | The height of the view, in number of cells
cellsHeight :: Int
cellsHeight = Constants.lobbiesPixelHeight `div` Constants.cps

-- | The width of the view, in number of cells
cellsWidth :: Int
cellsWidth = Constants.lobbiesCellWidth

mkModel :: Shared.Model -> Model.World
mkModel shared =
  Model.World {..}
  where
    position = Direction.Coord (24, 47) -- Initial position of character
    topLeft = Direction.Coord (13, 22)
    topology = Network.mkTopology $ concat Roads.points
    size = both Nat.intToNat (cellsWidth, cellsHeight)

both :: Bifunctor.Bifunctor p => (a -> d) -> p a a -> p d d
both f = Bifunctor.bimap f f

-- | Shifts the world's position by the given direction, if possible
shift :: Direction.T -> Model.World -> Model.World
shift dir m@Model.World {position, size = (width, height), topLeft = Direction.Coord (tlx, tly)} =
  case Direction.move dir position of
    Nothing -> m
    Just pos'@(Direction.Coord (x, y))
      | x < tlx -> m -- Too much to the left
      | x >= tlx + width -> m -- Too much to the right
      | y < tly -> m -- Too up
      | y >= tly + height -> m -- Too low
      | otherwise -> m {Model.position = pos'}
