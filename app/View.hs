{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module View where

import Constants
import Data.Map.Strict as Map
import Miso
import Miso.String
import Model

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x =
  div_
    [style_ globalStyle]
    [ div_ [style_ bgStyle] [backgroundCell],
      div_ [style_ $ cardStyle 3 3] [cardCreature (z + 1) assetFilenameHumanSpearman],
      div_ [style_ $ cardStyle 7 7] [cardCreature (z + 1) assetFilenameHumanGeneral]
    ]
  where
    z = 0
    cellSize = Constants.cellSize
    globalStyle =
      Map.fromList
        [ ("position", "relative"),
          ("top", "24px"),
          ("left", "24px")
        ]
    bgStyle =
      Map.fromList
        [ ("width", ms boardWidth <> "px"),
          ("height", ms boardHeight <> "px"),
          ("position", "absolute"),
          ("z-index", ms z)
        ]
    cardStyle xCellsOffset yCellsOffset =
      Map.fromList
        [ ("position", "relative"),
          ("display", "block"),
          ("top", ms (xCellsOffset * cellSize) <> "px"),
          ("left", ms (yCellsOffset * cellSize) <> "px")
        ]

backgroundCell :: View action
backgroundCell =
  img_ [width_ $ ms boardWidth, height_ $ ms boardHeight, src_ $ assetsPath "forest.png"]

imgCell :: MisoString -> View Action
imgCell filename =
  img_ [src_ $ assetsPath filename]

cardCreature :: Int -> MisoString -> View Action
cardCreature z creatureFilename =
  div_ [style_ divStyle] [div_ [style_ pictureStyle] [pictureCell], cardBackground z]
  where
    cellPixelSz = ms cellSize <> "px"
    divStyle = Map.fromList [("position", "relative"), ("width", cellPixelSz), ("height", cellPixelSz)]
    pictureStyle =
      Map.fromList
        [ ("z-index", ms $ z + 1),
          ("position", "absolute"),
          ("top", ms (cellSize `div` 4) <> "px"),
          ("left", ms ((cardWidth - cellSize) `div` 2) <> "px")
        ]
    pictureCell = imgCell creatureFilename

cardBackground :: Int -> View Action
cardBackground z =
  div_ [style_ divStyle] [genCell x y | x <- [0 .. 2], y <- [0 .. 3]]
  where
    cellPixelSz = ms cellSize <> "px"
    divStyle =
      Map.fromList [("position", "absolute")]
    genCell x y = div_ [style_ cardStyle] [imgCell assetFilenameBeigeBG]
      where
        cardStyle =
          Map.fromList
            [ ("width", cellPixelSz),
              ("height", cellPixelSz),
              ("position", "absolute"),
              ("display", "block"),
              ("z-index", ms z),
              ("left", ms (x * cellSize) <> "px"),
              ("top", ms (y * cellSize) <> "px")
              -- ("transform", "translateX(-128px) translateY(-128px)")
            ]
