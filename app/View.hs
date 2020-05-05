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
    [ div_ [style_ bgStyle] [bgCell],
      div_ [style_ cardStyle] [cardBgCell]
    ]
  where
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
        [ ("width", "24px"),
          ("height", "24px"),
          ("position", "absolute"),
          ("display", "block"),
          ("z-index", "2"),
          ("top", "24px"),
          ("left", "24px")
          -- ("transform", "translateX(-128px) translateY(-128px)")
        ]

bgCell :: View Action =
  img_ [width_ $ ms boardWidth, height_ $ ms boardHeight, src_ $ assetsPath "forest.png"]

cardBgCell :: View Action =
  img_ [width_ "24", height_ "24", src_ $ assetsPath "24x24_0_0.png"]
