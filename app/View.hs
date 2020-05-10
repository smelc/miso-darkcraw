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
  div_
    [style_ divStyle]
    [ div_ [style_ pictureStyle] [pictureCell],
      div_ [style_ statsStyle] [statsCell],
      cardBackground z
    ]
  where
    cellPixelSz = ms cellSize <> "px"
    divStyle = Map.fromList [("position", "relative"), ("width", cellPixelSz), ("height", cellPixelSz)]
    topMargin = cellSize `div` 4
    pictureStyle =
      Map.fromList
        [ ("z-index", ms $ z + 1),
          ("position", "absolute"),
          ("top", ms topMargin <> "px"),
          ("left", ms ((cardWidth - cellSize) `div` 2) <> "px")
        ]
    pictureCell :: View Action = imgCell creatureFilename
    statsStyle =
      Map.fromList
        [ ("z-index", ms $ z + 1),
          ("position", "absolute"),
          ("top", ms (topMargin + cellSize + topMargin) <> "px"),
          ("left", ms topMargin <> "px"),
          ("width", ms (cardWidth - (topMargin * 2)) <> "px"),
          ("height", ms cellSize <> "px")
        ]
    inStatsStyle =
      Map.fromList
        [ ("font-size", ms (cellSize `div` 2) <> "px"),
          ("font-family", "serif")
        ]
    statImgStyle :: Map MisoString MisoString = Map.fromList []
    statsCell :: View Action =
      div_
        [style_ inStatsStyle]
        [ text "1",
          imgCell assetFilenameHeart,
          text "1",
          imgCell assetFilenameSword
        ]

cardBackground :: Int -> View Action
cardBackground z =
  div_
    [style_ cardStyle]
    [ img_
        [ src_ $ assetsPath assetFilenameBeigeBG,
          width_ $ ms cardWidth,
          height_ $ ms cardHeight
        ]
    ]
  where
    divStyle = Map.fromList [("position", "absolute")]
    cardStyle =
      Map.fromList
        [ ("width", ms cardHeight <> "px"),
          ("height", ms cardHeight <> "px"),
          ("position", "absolute"),
          ("display", "block"),
          ("z-index", ms z),
          ("left", "0px"),
          ("top", "0px")
          -- ("transform", "translateX(-128px) translateY(-128px)")
        ]
