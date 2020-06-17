{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module View where

import Board
import Card
import Constants
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, isNothing, mapMaybe, maybeToList)
import Event
import Miso
import Miso.String
import Model
import Update
import Utils (style1_)
import ViewInternal

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel model@Model {board, interaction} =
  div_ [] $ [boardDiv, handDiv] ++ errView model zpp ++ [turnView model zpp]
  where
    (z, zpp) = (0, z + 1)
    globalLeftShift = (handPixelWidth - boardPixelWidth) `div` 2
    bgStyle :: Int -> Int -> Map.Map MisoString MisoString = zpwh z Absolute
    boardCards = boardToInPlaceCells (z + 1) model
    boardDiv =
      div_
        [style_ boardStyle]
        -- FIXME smelc replace backgroundCell with the "background-image" attr
        ( div_ [style_ $ bgStyle boardPixelWidth boardPixelHeight] [backgroundCell]
            : boardCards
        )
    boardStyle = zplt z Relative globalLeftShift 0
    backgroundCell =
      img_
        [ width_ $ ms boardPixelWidth,
          height_ $ ms boardPixelHeight,
          src_ $ assetsPath "forest.png",
          noDrag
        ]
    handCards = boardToInHandCells (z + 1) model
    handDiv =
      div_
        [style_ handStyle]
        -- FIXME smelc replace handCell with the "background-image" attr
        ( div_ [style_ $ bgStyle handPixelWidth handPixelHeight] [handCell]
            : handCards
        )
    handStyle = zplt z Relative 0 boardPixelHeight

boardToInPlaceCells ::
  -- | The z index
  Int ->
  Model ->
  [View Action]
boardToInPlaceCells z Model {board, interaction} =
  -- draw cards on table
  [ div_
      [style_ $ cardStyle x y]
      [cardCreature z (Just creature) False]
    | (pSpot, cSpot, creature) <- cardsInPlace,
      let (x, y) = cardCellsBoardOffset pSpot cSpot
  ]
    -- draw border around valid dragging targets if card in hand is:
    -- 1/ being hovered or 2/ being dragged
    ++ [ div_
           []
           [ nodeHtml "style" [] [keyframes r g b],
             div_
               [ style_ $ cardStyle x y, -- position the div
                 onDragEnter (DragEnter cSpot),
                 onDragLeave (DragLeave cSpot),
                 onDrop (AllowDrop True) DragEnd,
                 dummyOn "dragover",
                 style_ $
                   Map.fromList
                     [ ("animation-duration", "1s"),
                       ("animation-name", "pulse"),
                       ("animation-iteration-count", "1"),
                       ("animation-direction", "alternate"),
                       ("animation-timing-function", "ease-in-out")
                     ]
               ]
               [div_ [] []] -- empty divs, the point is that they have a border
           ]
         | case interaction of
             HoverInteraction {} -> True -- if card in hand is being hovered
             DragInteraction {} -> True -- if card in hand is being dragged
             ShowErrorInteraction _ -> False
             NoInteraction -> False,
           cSpot <- emptyPlayingPlayerSpots, -- on all empty spots
           let isDragTarget = case interaction of
                 DragInteraction Dragging {dragTarget} -> dragTarget == Just cSpot
                 _ -> False,
           let (x, y) = cardCellsBoardOffset playingPlayerSpot cSpot,
           let (r, g, b) = if isDragTarget then (255, 255, 0) else (0, 255, 0)
       ]
  where
    cardsInPlace :: [(PlayerSpot, CardSpot, Creature Core)] =
      boardToCardsInPlace board
    playingPlayerCardsSpots :: [CardSpot] =
      [c | (pSpot, c, _) <- cardsInPlace, pSpot == playingPlayerSpot]
    emptyPlayingPlayerSpots :: [CardSpot] =
      allCardsSpots \\ playingPlayerCardsSpots
    keyframes :: Int -> Int -> Int -> View Action = \r g b ->
      let rgba :: String = "rgba(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ",1);"
       in let result :: String = "@keyframes pulse { from { box-shadow: 0 0 0 0 " ++ rgba ++ " } to { box-shadow: 0 0 0 3px " ++ rgba ++ " } }"
           in toView result

boardToInHandCells ::
  -- | The z index
  Int ->
  Model ->
  [View Action]
boardToInHandCells z Model {board, interaction} =
  [ div_
      [ style_ $ cardStyle x 2,
        prop "draggable" True,
        onDragStart (DragStart i),
        onDragEnd DragEnd,
        class_ "card",
        onMouseEnter' "card" $ InHandMouseEnter i,
        onMouseLeave' "card" $ InHandMouseLeave i
      ]
      [cardCreature z (Just creature) beingHovered | not beingDragged]
    | (creature, i) <- Prelude.zip cards [HandIndex 0 ..],
      let x = cellsXOffset (unHandIndex i),
      let (beingHovered, beingDragged) =
            case interaction of
              HoverInteraction Hovering {hoveredCard} ->
                (hoveredCard == i, False)
              DragInteraction Dragging {draggedCard} ->
                (False, draggedCard == i)
              ShowErrorInteraction _ -> (False, False)
              NoInteraction -> (False, False)
  ]
  where
    cards :: [Creature Core] = boardToInHandCreaturesToDraw board
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
  Map.Map MisoString MisoString
cardStyle xCellsOffset yCellsOffset =
  pltwh Absolute xPixels yPixels cardPixelWidth cardPixelHeight
  where
    xPixels = xCellsOffset * cellPixelSize
    yPixels = yCellsOffset * cellPixelSize

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

handCell :: View Action
handCell =
  img_
    [ width_ $ ms handPixelWidth,
      src_ $ assetsPath "forest-hand.png",
      noDrag
    ]

imgCell :: MisoString -> View Action
imgCell filename =
  img_
    [ src_ $ assetsPath filename,
      noDrag
    ]

cardCreature ::
  -- | The z index
  Int ->
  -- | Whether a card should be drawn or solely a placeholder for drag target
  Maybe (Creature Core) ->
  -- | Whether this card is being hovered
  Bool ->
  View Action
cardCreature z creature hover =
  div_
    []
    $ [div_ [style_ pictureStyle] [pictureCell] | not placeholder]
      ++ [div_ [style_ statsStyle] [statsCell] | not placeholder]
      ++ [cardBackground z hover]
  where
    placeholder = isNothing creature
    topMargin = cellPixelSize `div` 4
    pictureStyle =
      zplt (z + 1) Absolute ((cardPixelWidth - cellPixelSize) `div` 2) topMargin
    pictureCell :: View Action = imgCell $ ms $ filename $ fromJust creature
    statsStyle = zpltwh (z + 1) Absolute topMargin top width cellPixelSize
      where
        width = cardPixelWidth - (topMargin * 2)
        top = topMargin + cellPixelSize + topMargin
    inStatsStyle =
      Map.fromList
        [ ("font-size", ms (cellPixelSize `div` 2) <> "px"),
          ("font-family", "serif"),
          ("display", "flex"),
          ("align-items", "center")
        ]
    statsCell :: View Action =
      div_
        [style_ inStatsStyle]
        [ text $ ms $ hp c,
          imgCell assetFilenameHeart,
          text $ ms $ attack c,
          imgCell assetFilenameSword
        ]
      where
        c = fromJust creature

cardBackground ::
  -- | The z index
  Int ->
  -- | Whether the card is being hovered
  Bool ->
  View Action
cardBackground z hover =
  div_
    [style_ cardStyle']
    [ img_
        [ src_ $ assetsPath assetFilenameBeigeBG,
          width_ $ ms cardPixelWidth,
          height_ $ ms cardPixelHeight,
          noDrag
        ]
    ]
  where
    cardStyle = zpwh z Absolute cardPixelWidth cardPixelHeight
    cardStyle' =
      if hover
        then Map.insert "outline" "3px solid red" cardStyle
        else cardStyle
