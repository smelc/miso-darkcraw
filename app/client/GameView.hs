{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Module to display an instance of the main view, i.e.
-- the battle between two players. It's the only module that
-- can depend on 'GameViewInternal'.
-- |
module GameView where

import Board
import Card
import Constants
import Control.Lens
import Data.Generics.Labels
import Data.Generics.Product
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing, mapMaybe, maybeToList)
import Event
import Game (enemySpots)
import GameViewInternal
import Miso hiding (at)
import Miso.String
import Model -- XXX tighten the imports?
import Update
import Utils (style1_)
import ViewInternal

-- | Constructs a virtual DOM from a game model
viewGameModel :: GameModel -> View Action
viewGameModel model@GameModel {board, interaction} =
  div_ [] $ [boardDiv, handDiv] ++ errView model zpp
  where
    (z, zpp) = (0, z + 1)
    boardCards = boardToInPlaceCells zpp model
    boardDiv = div_ [style_ boardStyle] $ turnView model zpp : boardCards
    boardStyle =
      Map.union (zpltwh z Relative 0 0 boardPixelWidth boardPixelHeight) $
        Map.fromList
          [("background-image", assetsUrl "forest.png")]
    handDiv = div_ [style_ handStyle] $ boardToInHandCells zpp model
    handStyle =
      Map.union (zpltwh z Relative 0 0 handPixelWidth handPixelHeight) $
        Map.fromList
          [("background-image", assetsUrl "forest-hand.png")]

boardToInPlaceCells ::
  -- | The z index
  Int ->
  GameModel ->
  [View Action]
boardToInPlaceCells z m@GameModel {anims, board, interaction} =
  [div_ [] $ bumpHtml : main]
  where
    main =
      [ div_
          ( [ style_ (Map.union (Map.fromList bounceStyle) $ cardPositionStyle x y),
              class_ "card",
              cardBoxShadowStyle (r, g, b) (borderWidth m pSpot cSpot) "ease-in-out"
            ]
              ++ case maybeCreature of
                Just _ ->
                  [ onMouseEnter' "card" $ GameAction' $ GameInPlaceMouseEnter pSpot cSpot,
                    onMouseLeave' "card" $ GameAction' $ GameInPlaceMouseLeave pSpot cSpot
                  ]
                Nothing ->
                  [ onDragEnter (GameAction' $ GameDragEnter cSpot),
                    onDragLeave (GameAction' $ GameDragLeave cSpot),
                    onDrop (AllowDrop True) $ GameAction' GameDrop,
                    dummyOn "dragover"
                  ]
          )
          $ [cardCreature z maybeCreature beingHovered | isJust maybeCreature]
            ++ deathFadeout attackEffect x y
        | (pSpot, cSpot, maybeCreature) <- boardToCardsInPlace board,
          let (x, y) = cardCellsBoardOffset pSpot cSpot,
          let beingHovered = interaction == GameHoverInPlaceInteraction pSpot cSpot,
          let attackEffect =
                anims ^. spotToLens pSpot . field' @"inPlace" . #unAttackEffects . ix cSpot,
          let bounceStyle =
                [ ("animation", bumpAnim <> " 0.5s ease-in-out")
                  | attackBump attackEffect
                ],
          let (r, g, b) =
                case interaction of
                  GameDragInteraction Dragging {dragTarget} | dragTarget == Just cSpot -> yellowRGB
                  _ -> greenRGB
      ]
    bumpAnim = "bump"
    bumpInit = "transform: translateY(0px);"
    bump50 = "transform: translateY(-" ++ show cellPixelSize ++ "px);"
    bumpKeyFrames = [keyframes bumpAnim bumpInit [(50, bump50)] bumpInit]
    bumpHtml = nodeHtml "style" [] bumpKeyFrames

boardToInHandCells ::
  -- | The z index
  Int ->
  GameModel ->
  [View Action]
boardToInHandCells z m@GameModel {board, interaction, playingPlayer} =
  [ div_
      [ style_ $ cardPositionStyle' x y,
        prop "draggable" True,
        onDragStart (GameAction' $ GameDragStart i),
        onDragEnd $ GameAction' GameDrop,
        class_ "card",
        onMouseEnter' "card" $ GameAction' $ GameInHandMouseEnter i,
        onMouseLeave' "card" $ GameAction' $ GameInHandMouseLeave i
      ]
      [cardCreature z (Just creature) beingHovered | not beingDragged]
    | (creature, i) <- Prelude.zip cards [HandIndex 0 ..],
      let x = pixelsXOffset (unHandIndex i),
      let y = 2 * cellPixelSize,
      let (beingHovered, beingDragged) =
            case interaction of
              GameHoverInteraction Hovering {hoveredCard} ->
                (hoveredCard == i, False)
              GameDragInteraction Dragging {draggedCard} ->
                (False, draggedCard == i)
              GameShowErrorInteraction _ -> (False, False)
              _ -> (False, False)
  ]
    ++ [stackView m z]
  where
    pLens = spotToLens playingPlayer
    cards :: [Creature Core] = boardToInHandCreaturesToDraw board pLens
    pixelsXOffset i
      | i == 0 = (boardPixelWidth - cardPixelWidth) `div` 2 -- center
      | i == 1 = pixelsXOffset 0 - xshift -- shift to the left compared to the center
      | i == 2 = pixelsXOffset 0 + xshift -- shift to the right compared to the center
      | i `mod` 2 == 0 = xshift + pixelsXOffset (i - 2) -- iterate
      | otherwise = pixelsXOffset (i - 2) - xshift -- iterate
      where
        xshift = cardCellWidth * cellPixelSize + (cardHCellGap * cellPixelSize) `div` 2

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
imgCell filename = img_ [src_ $ assetsPath filename, noDrag]

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
      Map.union flexLineStyle $
        Map.fromList
          [ ("font-size", ms (cellPixelSize `div` 2) <> "px"),
            ("font-family", "serif")
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
        then Map.insert "outline" (ms borderSize <> "px solid red") cardStyle
        else cardStyle
