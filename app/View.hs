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
viewModel model@Model {board, handFiddle} =
  div_ [] ([boardDiv, handDiv] ++ maybeToList draggedCardDiv)
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
    draggedCardDiv :: Maybe (View Action) =
      case handFiddle of
        Just (HandDragging i x y) ->
          let creatures = boardToInHandCreaturesToDraw board in
          let creature :: Creature Core = creatures !! unHandIndex i in
            Just $ div_
              [ style_ $ cardStyle 0 0 (Just x) (Just y) ]
              [cardCreature (z + 1) (Just creature) False]
        _ -> Nothing

boardToInPlaceCells ::
  -- | The z index
  Int ->
  Model ->
  [View Action]
boardToInPlaceCells z Model {board, handFiddle} =
  -- draw cards on table
  [ div_
      [ style_ $ cardStyle x y Nothing Nothing,
        onDrop (AllowDrop True) Drop
      ]
      [cardCreature z (Just creature) False]
    | (pSpot, cSpot, creature) <- board',
      let (x, y) = cardCellsBoardOffset pSpot cSpot
  ]
    -- draw border around valid dragging targets if card in hand is:
    -- 1/ being hovered or 2/ being dragged
    ++ [ div_
           [ style_ $ cardStyle x y Nothing Nothing, -- position the div
             style1_ "border" "3px solid #00FF00" -- draw the border
           ]
           [div_ [] []] -- empty divs, the point is that they have a border
         | case handFiddle of
             Just (HandHovering _) -> True -- if card in hand is being hovered
             Just HandDragging {} -> True -- if card in hand is being dragged
             _ -> False,
           cSpot <- emptyBottomSpots, -- on all empty spots
           let (x, y) = cardCellsBoardOffset PlayerBottom cSpot
       ]
  where
    board' :: [(PlayerSpot, CardSpot, Creature Core)] =
      boardToCardsInPlace board
    bottomCardsSpots :: [CardSpot] = [c | (PlayerBottom, c, _) <- board']
    emptyBottomSpots =
      [c | c <- allCardsSpots, c `notElem` bottomCardsSpots]

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
boardToInHandCells z Model {board, handFiddle} =
  [ div_
      [ style_ $ cardStyle x 2 Nothing Nothing,
        onDragXYEvent "drag" (DragXY i),
        class_ "card",
        onMouseEnter' "card" $ InHandMouseEnter i,
        onMouseLeave' "card" $ InHandMouseLeave i
      ]
      [cardCreature z (Just creature) beingHovered | not beingDragged]
    | (creature, i) <- Prelude.zip cards [HandIndex 0 ..],
      let x = cellsXOffset (unHandIndex i),
      let beingHovered = case handFiddle of
            Just (HandHovering j) -> j == i
            _ -> False,
      let beingDragged = case handFiddle of
            Just (HandDragging j _ _) -> j == i
            _ -> False
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
  -- | Optional horizontal offset, in PIXELS
  Maybe Int ->
  -- | Optional vertical offset, in PIXELS
  Maybe Int ->
  Map MisoString MisoString
cardStyle xCellsOffset yCellsOffset xPixsOffset yPixsOffset =
  Map.fromList
    [ ("position", "absolute"),
      -- ("display", "block"), doesn't seem required in the end
      ("width", ms cardPixelWidth <> "px"), -- required for drawing drag target
      ("height", ms cardPixelHeight <> "px"), -- required for drawing drag target
      ("left", ms xPixels <> "px"),
      ("top", ms yPixels <> "px")
    ]
  where
    xPixels = (xCellsOffset * cellPixelSize) + fromMaybe 0 xPixsOffset
    yPixels = (yCellsOffset * cellPixelSize) + fromMaybe 0 yPixsOffset

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
          height_ $ ms cardPixelHeight
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
