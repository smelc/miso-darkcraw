{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module View where

import Board
import Card
import Constants
import Data.Map.Strict as Map
import Debug.Trace
import Miso
import Miso.String
import Model

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel Model {board, uiCards} =
  div_
    [style_ globalStyle]
    (div_ [style_ bgStyle] [backgroundCell] : boardToCells (z + 1) board)
  where
    z :: Int = 0
    globalStyle =
      Map.fromList
        [ ("position", "relative"),
          ("top", "24px"),
          ("left", "24px")
        ]
    bgStyle =
      Map.fromList
        [ ("width", ms boardPixelWidth <> "px"),
          ("height", ms boardPixelHeight <> "px"),
          ("position", "absolute"),
          ("z-index", ms z)
        ]

-- div_ [style_ $ cardStyle 3 3] [cardCreature (z + 1) assetFilenameHumanSpearman],
-- div_ [style_ $ cardStyle 7 7] [cardCreature (z + 1) assetFilenameHumanGeneral]
boardToCells :: Int -> Board -> [View Action]
boardToCells z board =
  [ div_ [style_ $ cardStyle x y] [cardCreature z (ms $ filename creature)]
    | (pSpot, cSpot, creature) <- board',
      let (x, y) = trace (show (cellsOffset pSpot cSpot)) (cellsOffset pSpot cSpot)
  ]
  where
    board' :: [(PlayerSpot, CardSpot, Creature Core)] =
      boardToVisibleCards board
    cardStyle xCellsOffset yCellsOffset =
      Map.fromList
        [ ("position", "relative"),
          ("display", "block"),
          ("left", ms (xCellsOffset * cellPixelSize) <> "px"),
          ("top", ms (yCellsOffset * cellPixelSize) <> "px")
        ]

cellsOffset :: PlayerSpot -> CardSpot -> (Int, Int)
cellsOffset PlayerTop cardSpot =
  (offsetx + x, offsety + y)
  where
    (offsetx, offsety) = (3, 3) -- offset from background corner
    botyShift = cardCellHeight + cardVCellGap -- offset from top to bottom line
    xtop = cardCellWidth + cardHCellGap -- offset from left to middle
    xtopright = xtop * 2 -- offset from left to right
    (x, y) = case cardSpot of
      TopLeft -> (0, 0)
      Top -> (xtop, 0)
      TopRight -> (xtopright, 0)
      BottomLeft -> (0, botyShift)
      Bottom -> (xtop, botyShift)
      BottomRight -> (xtopright, botyShift)
cellsOffset PlayerBottom cardSpot =
  (offsetx + x, offsety + y)
  where
    (offsetx, offsety) = (3, 3 + (2 * cardCellHeight) + cardVCellGap + teamsVCellGap) -- offset from background corner
    topyShift = cardCellHeight + cardVCellGap
    xtop = cardCellWidth + cardHCellGap -- offset from left to middle
    xtopright = xtop * 2 -- offset from left to right
    (x, y) = case cardSpot of
      TopLeft -> (xtopright, topyShift) -- TopLeft is bottom right in bottom part
      Top -> (xtop, topyShift) -- Top is Bottom in bottom part
      TopRight -> (0, topyShift) -- TopRght is bottom left in bottom part
      BottomLeft -> (xtopright, 0) -- BottomLeft is top right in bottom part
      Bottom -> (xtop, 0) -- Bottom is Top in bottom part
      BottomRight -> (0, 0) -- BottomRight is Top Left in bottom part

backgroundCell :: View action
backgroundCell =
  img_ [width_ $ ms boardPixelWidth, height_ $ ms boardPixelHeight, src_ $ assetsPath "forest.png"]

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
    cellPixelSz = ms cellPixelSize <> "px"
    divStyle = Map.fromList [("position", "relative"), ("width", cellPixelSz), ("height", cellPixelSz)]
    topMargin = cellPixelSize `div` 4
    pictureStyle =
      Map.fromList
        [ ("z-index", ms $ z + 1),
          ("position", "absolute"),
          ("top", ms topMargin <> "px"),
          ("left", ms ((cardPixelWidth - cellPixelSize) `div` 2) <> "px")
        ]
    pictureCell :: View Action = imgCell creatureFilename
    statsStyle =
      Map.fromList
        [ ("z-index", ms $ z + 1),
          ("position", "absolute"),
          ("top", ms (topMargin + cellPixelSize + topMargin) <> "px"),
          ("left", ms topMargin <> "px"),
          ("width", ms (cardPixelWidth - (topMargin * 2)) <> "px"),
          ("height", ms cellPixelSize <> "px")
        ]
    inStatsStyle =
      Map.fromList
        [ ("font-size", ms (cellPixelSize `div` 2) <> "px"),
          ("font-family", "serif"),
          ("display", "flex"),
          ("align-items", "center")
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
          width_ $ ms cardPixelWidth,
          height_ $ ms cardPixelHeight
        ]
    ]
  where
    divStyle = Map.fromList [("position", "absolute")]
    cardStyle =
      Map.fromList
        [ ("width", ms cardPixelHeight <> "px"),
          ("height", ms cardPixelHeight <> "px"),
          ("position", "absolute"),
          ("display", "block"),
          ("z-index", ms z),
          ("left", "0px"),
          ("top", "0px")
          -- ("transform", "translateX(-128px) translateY(-128px)")
        ]
