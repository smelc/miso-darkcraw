{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ViewInternal
  ( errView,
    noDrag,
    Position (..),
    pltwh,
    zplt,
    zpltwh,
    zpwh,
  )
where

import Constants
import qualified Data.Map.Strict as Map
import Event
import Miso
import Miso.String
import Model
import Update
import Utils (style1_)

-- This module contains things used in View.hs
-- but which are likely rarely modified, to avoid
-- View.hs growing too much. Other modules besides
-- View should nod depend on this module.

errView :: Model -> Int -> [View Action]
errView model@Model {interaction} z =
  case interaction of
    HoverInteraction _ -> []
    DragInteraction _ -> []
    NoInteraction -> []
    ShowErrorInteraction msg -> [errView' msg]
  where
    errView' msg = div_ [style_ style'] [errBackgroundCell, textDiv msg]
    errBackgroundCell :: View Action =
      img_
        [ width_ $ ms (504 :: Int),
          height_ $ ms (168 :: Int),
          src_ $ assetsPath "errbox.png",
          noDrag
        ]
    style = zplt z Relative ((boardPixelWidth - 504) `div` 2) (boardPixelHeight `div` 2)
    style' = Map.union (Map.fromList [("opacity", "0.95")]) style
    textDiv msg = div_ [style_ textStylePairs] [text $ ms msg]
    textStylePairs =
      Map.fromList
        [("color", "#FF0000"), ("position", "relative")]

noDrag :: Attribute Action
noDrag = style_ (Map.fromList [("-webkit-user-drag", "none"), ("user-select", "none")])

-- * Now a lot of boring boilerplate

data Position = Absolute | Relative

instance Show Position where
  show Absolute = "absolute"
  show Relative = "relative"

-- | A style specifying the z-index, the position,
-- | the left margin (in pixels), and the top margin (in pixels)
zplt :: Int -> Position -> Int -> Int -> Map.Map MisoString MisoString
zplt z pos left top =
  Map.fromList
    [ ("z-index", ms z),
      ("position", ms $ show pos),
      ("left", ms left <> "px"),
      ("top", ms top <> "px")
    ]

-- | A style specifying the position, the top margin,
-- | the left margin, the width, and the height. All sizes are in pixels
pltwh :: Position -> Int -> Int -> Int -> Int -> Map.Map MisoString MisoString
pltwh pos left top width height =
  Map.fromList
    [ ("position", ms $ show pos),
      ("left", ms left <> "px"),
      ("top", ms top <> "px"),
      ("width", ms width <> "px"),
      ("height", ms height <> "px")
    ]

-- | A style specifying the z-index, the position, the left margin,
-- | the top margin, the width, and the height. All sizes are in pixels
zpltwh ::
  Int ->
  Position ->
  Int ->
  Int ->
  Int ->
  Int ->
  Map.Map MisoString MisoString
zpltwh z pos left top width height =
  Map.fromList
    [ ("z-index", ms z),
      ("position", ms $ show pos),
      ("top", ms top <> "px"),
      ("left", ms left <> "px"),
      ("width", ms width <> "px"),
      ("height", ms height <> "px")
    ]

-- | A style specifying the z-index, the position, the width (in pixels), and
-- | the height (in pixels)
zpwh :: Int -> Position -> Int -> Int -> Map.Map MisoString MisoString
zpwh z pos width height =
  Map.fromList
    [ ("z-index", ms z),
      ("position", ms $ show pos),
      ("width", ms width <> "px"),
      ("height", ms height <> "px")
    ]
