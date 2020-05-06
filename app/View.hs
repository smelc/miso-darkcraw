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
      div_ [style_ cardStyle] [cardBackground 2]
    ]
  where
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
          ("z-index", "1")
        ]
    cardStyle =
      Map.fromList
        [ ("position", "absolute"),
          ("display", "block"),
          ("z-index", "2"),
          ("top", ms (3 * cellSize) <> "px"),
          ("left", ms (3 * cellSize) <> "px")
        ]

backgroundCell :: View action
backgroundCell =
  img_ [width_ $ ms boardWidth, height_ $ ms boardHeight, src_ $ assetsPath "forest.png"]

cardBgCell :: View Action
cardBgCell =
  img_ [src_ $ assetsPath "24x24_0_2.png"]

cardBackground :: Int -> View Action
cardBackground z =
  div_ [style_ divStyle] [genCell x y | x <- [0 .. 2], y <- [0 .. 3]]
  where
    cellPixelSz = ms cellSize <> "px"
    divStyle =
      Map.fromList [("position", "relative")]
    genCell x y = div_ [style_ cardStyle] [cardBgCell]
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
