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
import Data.Maybe (fromJust, fromMaybe, isNothing, mapMaybe, maybeToList)
import Event
import Miso
import Miso.String
import Model
import Update
import Utils (style1_)

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel model@Model {board, interaction} =
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
    boardCards = boardToInPlaceCells (z + 1) model
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
    | (pSpot, cSpot, creature) <- board',
      let (x, y) = cardCellsBoardOffset pSpot cSpot
  ]
    -- draw border around valid dragging targets if card in hand is:
    -- 1/ being hovered or 2/ being dragged
    ++ [ div_
           [ style_ $ cardStyle x y, -- position the div
             style1_ "border" ("3px solid " <> borderColor), -- draw the border
             onDrop (AllowDrop True) Drop,
             onDragEnter (DragEnter cSpot),
             onDragLeave (DragLeave cSpot)
           ]
           [div_ [] []] -- empty divs, the point is that they have a border
         | case interaction of
             Just HoverInteraction {} -> True -- if card in hand is being hovered
             Just DragInteraction {} -> True -- if card in hand is being dragged
             _ -> False,
           cSpot <- emptyPlayingPlayerSpots, -- on all empty spots
           let isDragTarget = case interaction of
                 -- Would this deep deconstruction benefit of a lens ?
                 Just (DragInteraction Dragging {dragTarget}) -> dragTarget == Just cSpot
                 _ -> False,
           let (x, y) = cardCellsBoardOffset playingPlayerSpot cSpot,
           let borderColor = if isDragTarget then "#FFFF00" else "#00FF00"
       ]
  where
    board' :: [(PlayerSpot, CardSpot, Creature Core)] =
      boardToCardsInPlace board
    playingPlayerCardsSpots :: [CardSpot] =
      [c | (pSpot, c, _) <- board', pSpot == playingPlayerSpot]
    emptyPlayingPlayerSpots =
      [c | c <- allCardsSpots, c `notElem` playingPlayerCardsSpots]

boardToInHandCreaturesToDraw :: Board -> [Creature 'Core]
boardToInHandCreaturesToDraw board =
  cards
  where
    board' :: [Card Core] =
      Prelude.map snd
        $ Prelude.filter ((== PlayerBottom) . fst)
        $ boardToCardsInHand board
    cards :: [Creature Core] =
      let filter = \case
            CreatureCard c -> Just c
            NeutralCard _ -> Nothing
            ItemCard _ -> Nothing
       in Data.Maybe.mapMaybe filter board'

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
              Just (HoverInteraction Hovering {hoveredCard}) ->
                (hoveredCard == i, False)
              Just (DragInteraction Dragging {draggedCard}) ->
                (False, draggedCard == i)
              Nothing -> (False, False)
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
  Map MisoString MisoString
cardStyle xCellsOffset yCellsOffset =
  Map.fromList
    [ ("position", "absolute"),
      -- ("display", "block"), doesn't seem required in the end
      ("width", ms cardPixelWidth <> "px"), -- required for drawing drag target
      ("height", ms cardPixelHeight <> "px"), -- required for drawing drag target
      ("left", ms xPixels <> "px"),
      ("top", ms yPixels <> "px")
    ]
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

noDrag = style_ (Map.fromList [("-webkit-user-drag", "none"), ("user-select", "none")])

backgroundCell :: View Action
backgroundCell =
  img_
    [ width_ $ ms boardPixelWidth,
      height_ $ ms boardPixelHeight,
      src_ $ assetsPath "forest.png",
      noDrag
    ]

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
      Map.fromList
        [ ("z-index", ms $ z + 1),
          ("position", "absolute"),
          ("top", ms topMargin <> "px"),
          ("left", ms ((cardPixelWidth - cellPixelSize) `div` 2) <> "px")
        ]
    pictureCell :: View Action = imgCell $ ms $ filename $ fromJust creature
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
    [style_ cardStyle]
    [ img_
        [ src_ $ assetsPath assetFilenameBeigeBG,
          width_ $ ms cardPixelWidth,
          height_ $ ms cardPixelHeight,
          noDrag
        ]
    ]
  where
    cardStyle =
      Map.fromList $
        [ ("width", ms cardPixelWidth <> "px"),
          ("height", ms cardPixelHeight <> "px"),
          ("position", "absolute"),
          ("display", "block"),
          ("z-index", ms z),
          ("left", "0px"),
          ("top", "0px")
        ]
          ++ [("border", "3px solid red") | hover]
