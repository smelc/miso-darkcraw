{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- This module contains generic things to be used in *View.hs
-- and *ViewInternal.hs files. Contrary to 'ViewInternal', it contains
-- things that are specific to the game; not solely pure HTML/CSS stuff.
-- |
module PCWViewInternal
  ( cardBoxShadowStyle,
    cardCreature,
    cardPositionStyle,
    cardPositionStyle',
  )
where

import Card (Creature (..), Phase (..), filename)
import Constants
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isNothing)
import Miso
import Miso.String
import Miso.Util ((=:))
import Update (Action)
import ViewInternal

cardBoxShadowStyle ::
  -- | The (r, g, b) of the border
  (Int, Int, Int) ->
  -- | The width of the border
  Int ->
  -- | The timing-function of the transition
  MisoString ->
  Attribute a
cardBoxShadowStyle (r, g, b) width timingFunction =
  style_ $
    Map.fromList
      [ ("box-shadow", "0 0 0 " <> ms width <> "px " <> rgba r g b),
        ("transition", "box-shadow"),
        ("transition-duration", "0.15s"),
        ("transition-timing-function", timingFunction)
      ]

-- | Div displaying a card
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

cardPositionStyle ::
  -- | The horizontal offset from the enclosing container, in number of cells
  Int ->
  -- | The vertical offset from the enclosing container, in number of cells
  Int ->
  Map.Map MisoString MisoString
cardPositionStyle xCellsOffset yCellsOffset =
  cardPositionStyle'
    (xCellsOffset * cellPixelSize)
    (yCellsOffset * cellPixelSize)

cardPositionStyle' ::
  -- | The horizontal offset from the enclosing container, in pixels
  Int ->
  -- | The vertical offset from the enclosing container, in pixels
  Int ->
  Map.Map MisoString MisoString
cardPositionStyle' xPixelsOffset yPixelsOffset =
  pltwh Absolute xPixelsOffset yPixelsOffset cardPixelWidth cardPixelHeight
