{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This module contains generic things to be used in *View.hs
-- and *ViewInternal.hs files. If you wanna add stuff really specific
-- to the game and that is used among various *View.hs files, put it
-- in 'PCWViewInternal.hs'
--
-- This module is the base for other View modules, meaning almost all other
-- *View* modules depend on this module.
-- |
module ViewInternal where

import qualified Constants
import Control.Monad.Writer
import Data.Function ((&))
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Miso hiding (at)
import Miso.String hiding (length, map, take, zip)
import Nat

newtype Styled a = Styled (Writer (Set.Set MisoString) a)
  deriving (Functor, Applicative, Monad, MonadWriter (Set.Set MisoString))

emitTopLevelStyle :: MisoString -> Styled ()
emitTopLevelStyle style = tell (Set.singleton style)

renderStyledView :: Styled (View a) -> View a
renderStyledView (Styled m) =
  div_
    []
    [ nodeHtml "style" [] (fmap text (Set.toList styles)),
      e
    ]
  where
    (e, styles) = runWriter m

-- | Data for creating a CSS animation
data AnimationData = AnimationData
  { animDataDelay :: Maybe MisoString,
    animDataDirection :: Maybe MisoString,
    animDataDuration :: MisoString,
    animDataFillMode :: Maybe MisoString,
    animDataIterationCount :: Maybe MisoString,
    animDataName :: MisoString,
    animDataTimingFunction :: MisoString
  }

-- | Creates an 'AnimationData' requiring the minimal fields
animationData ::
  -- | The animation's name
  MisoString ->
  -- | The animation's duration
  MisoString ->
  -- | The animation's timing function
  MisoString ->
  AnimationData
animationData animDataName animDataDuration animDataTimingFunction =
  AnimationData {..}
  where
    animDataDelay = Nothing -- "0s"
    animDataDirection = Nothing -- "normal"
    animDataFillMode = Nothing -- "none"
    animDataIterationCount = Nothing -- "1"

animDataToStyle :: AnimationData -> Attribute a
animDataToStyle AnimationData {..} =
  style_ $
    Map.empty
      & upd "animation-delay" animDataDelay
      & upd "animation-direction" animDataDirection
      & Map.insert "animation-duration" animDataDuration
      & upd "animation-iteration-count" animDataIterationCount
      & upd "animation-fill-mode" animDataFillMode
      & Map.insert "animation-name" animDataName
      & Map.insert "animation-timing-function" animDataTimingFunction
  where
    upd key value map =
      case value of
        Nothing -> map
        Just value -> Map.insert key value map

-- FIXME @smelc use Color.white instead
textMainColor :: MisoString
textMainColor = "#FFFFFF" -- white

textRawStyle :: [(MisoString, MisoString)]
textRawStyle = [("color", textMainColor)]

textStyle :: Map.Map MisoString MisoString
textStyle = Map.fromList textRawStyle

data Position = Absolute | Relative

-- TODO @smelc Use ToMisoString or alike instead?
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

imgCell :: MisoString -> View a
imgCell filename = img_ [src_ $ Constants.assetsPath filename, noDrag]

-- | An image with the given width and height, in pixels
imgCellwh :: MisoString -> Int -> Int -> Maybe Position -> View a
imgCellwh filename w h pos =
  -- Don't use 'px x', width and height are always in pixels, it'd
  -- make them considered as being 0.
  img_ $
    [src_ $ Constants.assetsPath filename, noDrag, width_ $ ms w, height_ $ ms h]
      ++ ( case pos of
             Nothing -> mempty
             Just pos -> [style_ $ "position" =: (ms $ show pos)]
         )

keyframes :: MisoString -> MisoString -> [(Int, String)] -> MisoString -> MisoString
keyframes name from steps to =
  "@keyframes " <> name <> "{ " <> tail <> " }"
  where
    from_ = "0% { " <> from <> " }"
    steps' :: String =
      Data.List.map (\(i, s) -> show i ++ "% { " ++ s ++ " }") steps
        & Data.List.intersperse " "
        & Data.List.concat
    to_ = "100% { " <> to <> " }"
    tail = from_ <> " " <> ms steps' <> " " <> to_

-- XXX smelc Get rid of this function altogether?
keyframed ::
  -- | How to build the element
  ([Attribute a] -> View a) ->
  -- | The keyframes, for example built with 'keyframes'
  MisoString ->
  -- | How to display the animation
  AnimationData ->
  Styled (View a)
keyframed e keyframes animData = do
  emitTopLevelStyle keyframes
  return $ e [animDataToStyle animData]

-- | @fade builder delay duration fade@ builds a view that fades
-- for @duration@ seconds after @delay@.
fade ::
  -- | How to build the element
  ([Attribute a] -> View a) ->
  -- | The delay at which the animation starts, in seconds.
  Maybe Nat ->
  -- | The duration, in seconds
  Nat ->
  -- | Whether it's a fadein ('True'), or a fadeout ('False')
  Constants.Fade ->
  Styled (View a)
fade builder delay duration =
  \case
    Constants.FadeIn -> go "opacity: 0;" "opacity: 1;" True
    Constants.FadeOut -> go "opacity: 1;" "opacity: 0;" False
    Constants.DontFade -> return $ builder []
  where
    go start end inOrOut =
      keyframed
        builder
        (keyframes animDataName start [] end)
        d
      where
        d@AnimationData {animDataName} = animData inOrOut
    duration' = ms (show duration ++ "s")
    animData inOrOut =
      (animationData ("fade_" <> suffix <> "_" <> duration') duration' "ease")
        { animDataDelay = fmap (addSecSuffix . ms . natToInt) delay,
          animDataFillMode = Just "forwards"
        }
      where
        suffix = if inOrOut then "in" else "out"
    addSecSuffix s = s <> "s"

textFrames :: MisoString -> (Int, MisoString, Bool) -> (Int, MisoString, Bool) -> MisoString
textFrames name (startFontSize, startColor, startTransparent) (endFontSize, endColor, endTransparent) =
  keyframes
    name
    (builder startFontSize startColor startTransparent)
    []
    (builder endFontSize endColor endTransparent)
  where
    builder font color transparent =
      "font-size: "
        <> ms (px font)
        <> "; color: "
        <> color
        <> "; opacity: "
        <> (if transparent then "0" else "1")
        <> ";"

-- | Animation to transition the size and the opacity (from opaque to transparent)
grow :: MisoString -> (Int, Int) -> (Int, Int) -> MisoString
grow name start end =
  keyframes name (mk start <> " opacity: 1;") [] (mk end <> " opacity: 0;")
  where
    mk (w, h) = "width: " <> px w <> "; height: " <> px h <> ";"

-- | Vertical wobbling
wobblev ::
  MisoString ->
  -- | Whether to wobble up or down
  Bool ->
  -- | The maximum x variation
  Int ->
  -- | The maximum y variation
  Int ->
  MisoString
wobblev name upOrDown x y =
  keyframes name from steps to
  where
    from = ms $ step (0 :: Int) (0 :: Int) "1"
    steps =
      [ (p, step x' ((round y') :: Int) opacity)
        | let steps = zip [15, 30, 45, 60, 85] [1 :: Int ..],
          let steps' = map (\(p, i) -> (p, if even i then x else -x)) steps,
          (p :: Int, x' :: Int) <- steps',
          let y' :: Float = (fromIntegral p / 100) * fromIntegral y,
          -- Take first three chars as in .eg. "0.3"
          let opacity = show ((100 / fromIntegral p) :: Double) & take 3
      ]
    to = ms $ step (0 :: Int) (if upOrDown then y else -y) "0"
    translateX i = "translateX(" ++ show i ++ "px)"
    translateY i = "translateY(" ++ ysign ++ show i ++ "px)"
    step x y o =
      "transform: " ++ translateX x ++ " " ++ translateY y ++ "; opacity: " ++ o ++ ";"
    ysign = if upOrDown then "-" else ""

noDrag :: Attribute a
noDrag = style_ $ "-webkit-user-drag" =: "none" <> "user-select" =: "none"

-- | A style specifing the horizontal margin (left and right) and vertical
-- | margin (top and bottom). Both sizes are in pixels
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
marginifyhv :: Int -> Int -> View a -> View a
marginifyhv h v view =
  div_ [style_ $ marginhv h v] [view]

-- | px i = ms i <> "px"
px :: Int -> MisoString
px i = ms i <> "px"

rgba :: Int -> Int -> Int -> MisoString
rgba r g b =
  "rgba(" <> ms r <> "," <> ms g <> "," <> ms b <> ",1)"

-- | Styled text, specifying the z-index and the text
stytextz :: Int -> MisoString -> View a
stytextz z txt = stytextzhv z txt 0 0

-- | Styled text, specifying the z-index, the text, the left and right margin
-- | (in pixels) and the top and bottom margin (in pixels)
stytextzhv :: Int -> MisoString -> Int -> Int -> View a
stytextzhv z txt h v =
  div_
    [ style_ $ Map.fromList textRawStyle,
      style_ $ "z-index" =: ms z,
      style_ $ marginhv h v
    ]
    [text txt]

-- | Styled text, specifying the z-index, the text, the left and right margin
-- | (in pixels) and the top and bottom margin (in pixels)
stytextztrbl :: Int -> MisoString -> Int -> Int -> Int -> Int -> View a
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
  zprbwh z pos (cps * right) (cps * bot) cps cps
  where
    cps = Constants.cps

-- | A style specifying the z-index, the position,
-- | the left margin (in pixels), and the top margin (in pixels)
zplt :: Int -> Position -> Int -> Int -> Map.Map MisoString MisoString
zplt z pos left top =
  "z-index" =: ms z
    <> "position" =: ms (show pos)
    <> "left" =: px left
    <> "top" =: px top

-- | A style specifying the position, the top margin,
-- | the left margin, the width, and the height. All sizes are in pixels
pltwh :: Position -> Int -> Int -> Int -> Int -> Map.Map MisoString MisoString
pltwh pos left top width height =
  "position" =: ms (show pos)
    <> "left" =: px left
    <> "top" =: px top
    <> "width" =: px width
    <> "height" =: px height

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
zprb z pos right bot =
  "z-index" =: ms z
    <> "position" =: ms (show pos)
    <> "right" =: px right
    <> "bottom" =: px bot

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
  "z-index" =: ms z
    <> "position" =: ms (show pos)
    <> "right" =: px right
    <> "bottom" =: px bottom
    <> "width" =: px width
    <> "height" =: px height

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
