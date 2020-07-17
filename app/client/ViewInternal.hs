{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This module contains generic things to be used in *View.hs
-- and *ViewInternal.hs files.
-- |
module ViewInternal where

import Constants (cellPixelSize)
import qualified Data.Map.Strict as Map
import Miso hiding (at)
import Miso.String hiding (length)
import Miso.Util ((=:))
import Update (Action (..))

buttonStyle :: Attribute action
buttonStyle =
  style_
    $ Map.fromList
    $ [ ("background-color", "transparent"), -- no background
        ("border", "2px solid " <> textMainColor), -- white not-shadowed border
        ("outline", "none") -- don't highlight that it has been pressed
      ]
      ++ textRawStyle

textMainColor :: MisoString
textMainColor = "#FFFFFF" -- white

textRawStyle :: [(MisoString, MisoString)]
textRawStyle = [("color", textMainColor)]

-- | Dummy [onWithOptions] instance.
-- | See https://github.com/dmjio/miso/issues/478
dummyOn ::
  -- | One of "dragenter" or "dragover"
  MisoString ->
  Attribute Action
dummyOn str =
  onWithOptions
    defaultOptions {preventDefault = True}
    str
    emptyDecoder
    (\() -> NoOp)

rgba :: Int -> Int -> Int -> MisoString
rgba r g b =
  "rgba(" <> ms r <> "," <> ms g <> "," <> ms b <> ",1)"

data Position = Absolute | Relative

instance Show Position where
  show Absolute = "absolute"
  show Relative = "relative"

flexColumnStyle :: Map.Map MisoString MisoString
flexColumnStyle =
  Map.fromList
    [ ("display", "flex"),
      ("flex-direction", "column"),
      ("align-items", "center")
    ]

flexLineStyle :: Map.Map MisoString MisoString
flexLineStyle =
  Map.fromList
    [ ("display", "flex"),
      ("align-items", "center")
    ]

-- | A style specifing the left and right margin and the top and bottom margin
-- | Both sizes are in pixels
marginhv :: Int -> Int -> Map.Map MisoString MisoString
marginhv h v = Map.singleton "margin" (ms v <> "px " <> ms h <> "px")

-- | A style specifying the z-index, the position,
-- | the right margin (in cells), and the bottom margin (in pixels) of a tile
-- | i.e. of a rectangle of size 'cellPixelSize'.
tilerb :: Int -> Position -> Int -> Int -> Map.Map MisoString MisoString
tilerb z pos right bot =
  zprbwh z pos (cellPixelSize * right) (cellPixelSize * bot) cellPixelSize cellPixelSize

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

-- | A style specifying the z-index, the position, the right margin,
-- | the bottom margin. All sizes are in pixels
zprb ::
  Int ->
  Position ->
  Int ->
  Int ->
  Map.Map MisoString MisoString
zprb z pos left top =
  Map.fromList
    [ ("z-index", ms z),
      ("position", ms $ show pos),
      ("right", ms top <> "px"),
      ("bottom", ms left <> "px")
    ]

-- | A style specifying the z-index, the position, the right margin,
-- | the bottom margin, the width, and the height. All sizes are in pixels
zprbwh ::
  Int ->
  Position ->
  Int ->
  Int ->
  Int ->
  Int ->
  Map.Map MisoString MisoString
zprbwh z pos right bottom width height =
  Map.fromList
    [ ("z-index", ms z),
      ("position", ms $ show pos),
      ("right", ms right <> "px"),
      ("bottom", ms bottom <> "px"),
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
