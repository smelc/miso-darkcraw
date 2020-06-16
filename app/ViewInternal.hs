{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ViewInternal
  ( errView,
    noDrag,
    Position (..),
    pltwh,
    turnView,
    zplt,
    zpltwh,
    zpwh,
  )
where

import Board (PlayerSpot (..))
import Constants
import qualified Data.Map.Strict as Map
import Event
import Miso
import Miso.String
import Model
import Turn (turnToInt, turnToPlayerSpot)
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
    ShowErrorInteraction msg ->
      [div_ [style_ errViewStyle] $ textView msg : feedbackViews]
  where
    (width, height) = (504, 168)
    left = (boardPixelWidth - width) `div` 2
    top = boardPixelHeight `div` 2
    errViewStyle =
      Map.union (zpltwh z Relative left top width height) $
        Map.fromList
          [ ("display", "flex"),
            ("align-items", "center"),
            ("justify-content", "center"),
            ("flex-direction", "column"),
            ("background-image", assetsUrl "errbox.png")
          ]
    errViewStyle' = Map.union (Map.fromList [("opacity", "0.9")]) errViewStyle
    textView msg = div_ [style_ textStylePairs] [text $ ms msg]
    textWidth = width - (cellPixelSize * 2)
    textWidthAttr = ("width", ms textWidth <> "px")
    -- XXX It'd be better to center the error message (like the feedback text).
    -- This is the case if the width is NOT specified.
    textStylePairs = Map.fromList [("color", "#FF0000"), textWidthAttr]
    feedbackViews :: [View Action] =
      [ br_ [],
        text "Please copy/paste this error in a comment of ",
        a_ [href_ itch] [text itch]
      ]
    itch = "https://hgames.itch.io/darkcraw"

turnView :: Model -> Int -> View Action
turnView model@Model {turn} z =
  div_ [style_ turnViewStyle, style_ textStylePairs] [line1, line2]
  where
    (width, height) = (turnPixelWidth, turnPixelHeight)
    left = boardPixelWidth - width
    top = boardPixelHeight - height
    turnViewStyle =
      Map.union (zpltwh z Relative left top width height) $
        Map.fromList
          [ ("display", "flex"),
            ("align-items", "center"),
            ("justify-content", "center"),
            ("flex-direction", "column"),
            ("background-image", assetsUrl "turn.png")
          ]
    textStylePairs = Map.fromList [("color", "#FFFFFF")]
    line1 :: View Action = text $ "Turn " <> ms (turnToInt turn)
    playerImgY =
      case turnToPlayerSpot turn of
        PlayerTop -> "1"
        PlayerBottom -> "2"
    line2ImgSize = cellPixelSize
    topMargin = cellPixelSize `div` 2
    line2 :: View Action =
      div_
        [style_ $ Map.singleton "margin-top" $ ms topMargin <> "px"]
        [ nodeHtml "style" [] ["@keyframes pulse { from { outline: 0px solid #00FF00; } to { outline: 4px solid #00FF00; } }"],
          img_ $
            [ src_ $ assetsPath "24x24_" <> playerImgY <> "_2.png",
              width_ $ ms line2ImgSize,
              height_ $ ms line2ImgSize,
              noDrag
            ]
              ++ [ style_ $
                     Map.fromList
                       [ ("animation-duration", "1s"),
                         ("animation-name", "pulse"),
                         ("animation-iteration-count", "infinite"),
                         ("animation-direction", "alternate")
                       ]
                 ]
        ]

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
