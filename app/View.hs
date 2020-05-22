{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module View where

import Board
import Card
import Constants
import Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Miso
import Miso.String
import Model

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel model@Model {board, uiCards} =
  div_ [] [boardDiv, handDiv]
  where
    z :: Int = 0
    globalLeftShift = (handPixelWidth - boardPixelWidth) `div` 2
    boardStyle =
      Map.fromList
        [ ("position", "relative"),
          ("left", ms globalLeftShift <> "px"),
          ("top", "0px")
        ]
    bgStyle pixelWidth pixelHeight =
      Map.fromList
        [ ("width", ms pixelWidth <> "px"),
          ("height", ms pixelHeight <> "px"),
          ("position", "absolute"),
          ("z-index", ms z)
        ]
    boardCards = boardToInPlaceCells (z + 1) board
    boardDiv =
      div_
        [style_ boardStyle]
        ( div_ [style_ $ bgStyle boardPixelWidth boardPixelHeight] [backgroundCell]
            : boardCards
        )
    handCards = boardToInHandCells (z + 1) model
    handDiv =
      div_
        [style_ handStyle]
        ( div_ [style_ $ bgStyle handPixelWidth handPixelHeight] [handCell]
            : handCards
        )
    handStyle =
      Map.fromList
        [ ("position", "relative"),
          ("top", ms boardPixelHeight <> "px")
        ]

boardToInPlaceCells :: Int -> Board -> [View Action]
boardToInPlaceCells z board =
  [ div_ [style_ $ cardStyle x y] [cardCreature z creature False]
    | (pSpot, cSpot, creature) <- board',
      let (x, y) = cardCellsBoardOffset pSpot cSpot
  ]
  where
    board' :: [(PlayerSpot, CardSpot, Creature Core)] =
      boardToCardsInPlace board

boardToInHandCells :: Int -> Model -> [View Action]
boardToInHandCells z Model {board, handHover} =
  [ div_
      [ style_ $ cardStyle x 2,
        onMouseOver $ InHandMouseEnter i,
        onMouseLeave $ InHandMouseLeave i
      ]
      [cardCreature z creature beingHovered]
    | (creature, i) <- Prelude.zip cards' [0 ..],
      let x = cellsXOffset i,
      let beingHovered = handHover == Just i
  ]
  where
    board' :: [(PlayerSpot, Card Core)] = boardToCardsInHand board
    cards :: [Card Core] = [c | (p, c) <- board', p == PlayerBottom]
    cards' :: [Creature Core] =
      let filter = \case
            CreatureCard c -> Just c
            NeutralCard _ -> Nothing
            ItemCard _ -> Nothing
       in Data.Maybe.mapMaybe filter cards
    cellsXOffset i
      | i == 0 = boardToLeftCardCellsOffset + (cardCellWidth * 2) -- center
      | i == 1 = cellsXOffset 0 - xshift -- shift to the left compared to the center
      | i == 2 = cellsXOffset 0 + xshift -- shift to the right compared to the center
      | i `mod` 2 == 0 = - xshift + cellsXOffset (i - 2) -- iterate
      | otherwise = xshift + cellsXOffset (i - 2) -- iterate
      where
        xshift = cardCellWidth + cardHCellGap

cardStyle ::
  -- | The horizontal offset from the enclosing container, in number of cells
  Int ->
  -- | The vertical offset from the enclosing container, in number of cells
  Int ->
  Map MisoString MisoString
cardStyle xCellsOffset yCellsOffset =
  Map.fromList
    [ ("position", "absolute"),
      ("display", "block"),
      ("width", ms cardPixelWidth <> "px"),
      ("height", ms cardPixelHeight <> "px"),
      ("left", ms (xCellsOffset * cellPixelSize) <> "px"),
      ("top", ms (yCellsOffset * cellPixelSize) <> "px")
    ]

cardCellsBoardOffset :: PlayerSpot -> CardSpot -> (Int, Int)
cardCellsBoardOffset PlayerTop cardSpot =
  (offsetx + x, offsety + y)
  where
    (offsetx, offsety) =
      (boardToLeftCardCellsOffset, 3) -- offset from background corner
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
cardCellsBoardOffset PlayerBottom cardSpot =
  (offsetx + x, offsety + y)
  where
    (offsetx, offsety) =
      ( boardToLeftCardCellsOffset,
        3 + (2 * cardCellHeight) + cardVCellGap + teamsVCellGap -- offset from background corner
      )
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

backgroundCell :: View Action
backgroundCell =
  img_
    [ width_ $ ms boardPixelWidth,
      height_ $ ms boardPixelHeight,
      src_ $ assetsPath "forest.png"
    ]

handCell :: View Action
handCell =
  img_
    [ width_ $ ms handPixelWidth,
      src_ $ assetsPath "forest-hand.png"
    ]

imgCell :: MisoString -> View Action
imgCell filename =
  img_ [src_ $ assetsPath filename]

cardCreature ::
  Int ->
  Creature Core ->
  -- | Whether this card is being hovered
  Bool ->
  View Action
cardCreature z creature hover =
  div_
    [style_ divStyle]
    [ div_ [style_ pictureStyle] [pictureCell],
      div_ [style_ statsStyle] [statsCell],
      cardBackground z hover
    ]
  where
    cellPixelSz = ms cellPixelSize <> "px"
    divStyle = Map.fromList [("position", "relative")]
    topMargin = cellPixelSize `div` 4
    pictureStyle =
      Map.fromList
        [ ("z-index", ms $ z + 1),
          ("position", "absolute"),
          ("top", ms topMargin <> "px"),
          ("left", ms ((cardPixelWidth - cellPixelSize) `div` 2) <> "px")
        ]
    pictureCell :: View Action = imgCell $ ms $ filename creature
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
        [ text $ ms $ hp creature,
          imgCell assetFilenameHeart,
          text $ ms $ attack creature,
          imgCell assetFilenameSword
        ]

cardBackground ::
  Int ->
  -- | Whether the card is being hovered
  Bool ->
  View Action
cardBackground z hover =
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
      Map.fromList $
        [ ("width", ms cardPixelWidth <> "px"),
          ("height", ms cardPixelHeight <> "px"),
          ("position", "absolute"),
          ("display", "block"),
          ("z-index", ms z),
          ("left", "0px"),
          ("top", "0px")
          -- ("transform", "translateX(-128px) translateY(-128px)")
        ]
          ++ [("border", "3px solid red") | hover]
