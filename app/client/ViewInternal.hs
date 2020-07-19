{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This module contains generic things to be used in *View.hs
-- and *ViewInternal.hs files.
-- |
module ViewInternal where

import Constants (assetsPath, cellPixelSize)
import Control.Lens
import Data.Function ((&))
import Data.List
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

-- TODO smelc carry me over with a monad
textMainColor :: MisoString
textMainColor = "#FFFFFF" -- white

-- TODO smelc carry me over with a monad
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

img_' :: MisoString -> View Action
img_' filename = img_ [src_ $ assetsPath filename, noDrag]

keyframes :: MisoString -> MisoString -> [(Int, String)] -> MisoString -> View m
keyframes name from steps to =
  text $ "@keyframes " <> name <> "{ " <> tail <> " }"
  where
    from_ = "from { " <> from <> " }"
    steps' :: String =
      Data.List.map (\(i, s) -> show i ++ "% { " ++ s ++ " }") steps
        & Data.List.intersperse " "
        & Data.List.concat
    to_ = "to { " <> to <> " }"
    tail = from_ <> " " <> ms steps' <> " " <> to_

keyframed ::
  -- | How to build the element
  ([Attribute Action] -> View Action) ->
  -- | The 'from' attribute
  MisoString ->
  -- | The 'to' attribute
  MisoString ->
  -- | The attributes `animation-name`, `animation-iteration-count` and
  -- | `animate-timing-function`
  (MisoString, MisoString, MisoString) ->
  -- | The attribute `animation-direction`
  Maybe MisoString ->
  -- | The attribute `animation-fill-mode`
  Maybe MisoString ->
  [View Action]
keyframed e from to (name, iterationCount, timingFunction) direction fillMode =
  [ nodeHtml
      "style"
      []
      [keyframes name from [] to],
    e
      [ noDrag,
        style_ $
          Map.empty
            & at "animation-duration" ?~ "1s"
            & at "animation-name" ?~ name
            & at "animation-iteration-count" ?~ iterationCount
            & at "animation-timing-function" ?~ timingFunction
            & at "animation-direction" .~ direction
            & at "animation-fill-mode" .~ fillMode
      ]
  ]

noDrag :: Attribute Action
noDrag = style_ (Map.fromList [("-webkit-user-drag", "none"), ("user-select", "none")])

-- | A style specifing the left and right margin and the top and bottom margin
-- | Both sizes are in pixels
marginhv :: Int -> Int -> Map.Map MisoString MisoString
marginhv h v = margintrbl v h v h

-- | A style specifing the top, right, bottom, and left margins.
-- | All sizes are in pixels
margintrbl :: Int -> Int -> Int -> Int -> Map.Map MisoString MisoString
margintrbl t r b l =
  Map.singleton
    "margin"
    (ms t <> "px " <> ms r <> "px " <> ms b <> "px " <> ms l <> "px")

-- | Surrounds an element with a div specifying the left and right margin
-- | and the top and bottom margin. All sizes are in pixels.
marginifyhv :: Int -> Int -> View Action -> View Action
marginifyhv h v view =
  div_ [style_ $ marginhv h v] [view]

-- | px i = ms i <> "px"
px :: Int -> MisoString
px i = ms i <> "px"

-- | Styled text, specifying the z-index, the text, the left and right margin
-- | (in pixels) and the top and bottom margin (in pixels)
stytextzhv :: Int -> MisoString -> Int -> Int -> View action
stytextzhv z txt h v =
  div_
    [ style_ $ Map.fromList textRawStyle,
      style_ $ "z-index" =: ms z,
      style_ $ marginhv h v
    ]
    [text txt]

-- | Styled text, specifying the z-index, the text, the left and right margin
-- | (in pixels) and the top and bottom margin (in pixels)
stytextztrbl :: Int -> MisoString -> Int -> Int -> Int -> Int -> View action
stytextztrbl z txt t r b l =
  div_
    [ style_ $ Map.fromList textRawStyle,
      style_ $ "z-index" =: ms z,
      style_ $ margintrbl t r b l
    ]
    [text txt]

-- | A style specifying the z-index, the position,
-- | the right margin (in cells), and the bottom margin (in pixels) of a tile
-- | i.e. of a rectangle of size 'cellPixelSize'.
tilezprb :: Int -> Position -> Int -> Int -> Map.Map MisoString MisoString
tilezprb z pos right bot =
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
