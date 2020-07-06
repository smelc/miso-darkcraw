{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ViewInternal
  ( borderWidth,
    cardBoxShadowStyle,
    cardPositionStyle,
    deathFadeout,
    dummyOn,
    errView,
    noDrag,
    Position (..),
    pltwh,
    turnView,
    zplt,
    zpltwh,
    zprbwh,
    zpwh,
  )
where

import Board
import Card
import Constants
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Event
import Game (enemySpots)
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
    ShowErrorInteraction msg ->
      [div_ [style_ errViewStyle] $ textView msg : feedbackViews]
    _ -> []
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
        a_ [href_ itchURL] [text itchURL]
      ]

turnView :: Model -> Int -> View Action
turnView model@Model {turn} z =
  div_ [style_ turnViewStyle, style_ textStylePairs] [line1, line2, line3]
  where
    turnViewStyle =
      Map.union (zprbwh z Absolute 0 0 turnPixelWidth turnPixelHeight) $
        Map.fromList
          [ ("display", "flex"),
            ("align-items", "center"),
            ("justify-content", "center"),
            ("flex-direction", "column"),
            ("background-image", assetsUrl "turn.png")
          ]
    mainColor = "#FFFFFF"
    textStylePairs = Map.fromList [("color", mainColor)]
    line1 :: View Action = text $ "Turn " <> ms (turnToInt turn)
    playerImgY =
      case turnToPlayerSpot turn of
        PlayerTop -> "1"
        PlayerBottom -> "2"
    line2ImgSize = cellPixelSize
    topMargin = cellPixelSize `div` 2
    topMarginAttr = style_ $ Map.singleton "margin-top" $ ms topMargin <> "px"
    line2 :: View Action =
      div_
        [topMarginAttr]
        [ nodeHtml "style" [] ["@keyframes pulse { from { box-shadow: 0 0 0 0 rgba(0,255,0,1); } to { box-shadow: 0 0 0 3px rgba(0,255,0,1); } }"],
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
                         ("animation-direction", "alternate"),
                         ("animation-timing-function", "ease-in-out")
                       ]
                 ]
        ]
    line3Style =
      style_ $
        Map.fromList
          [ ("background-color", "transparent"), -- no background
            ("border", "2px solid " <> mainColor), -- white not-shadowed border
            ("outline", "none") -- don't highlight that it has been pressed
          ]
    line3 :: View Action =
      button_
        [topMarginAttr, onClick EndTurn, line3Style]
        [div_ [style_ textStylePairs] [text "End Turn"]]

-- draw border around some cards if:
-- 1/ card in hand is being hovered or dragged -> draw borders around
--    valid drag targets
-- or 2/ card in place is being hovered -> draw borders around cards
--       be attacked from this card== playingPlayerSpot,
borderWidth :: Model -> PlayerSpot -> CardSpot -> Int
borderWidth Model {board, interaction} pSpot cSpot =
  case interaction of
    DragInteraction _ | emptyPlayingPlayerSpot -> 3
    HoverInteraction _ | emptyPlayingPlayerSpot -> 3
    HoverInPlaceInteraction pSpot' cSpotHovered ->
      let attacker = boardToInPlaceCreature board (spotToLens pSpot') cSpotHovered
       in let skills' =
                case attacker of
                  Nothing -> [] -- case should not happen but we handle it
                  Just attacker -> fromMaybe [] $ skills attacker
           in if pSpot /= pSpot' && cSpot `elem` enemySpots skills' cSpotHovered
                then borderSize
                else 0
    _ -> 0
  where
    allInPlace :: [(PlayerSpot, CardSpot, Maybe (Creature Core))] =
      boardToCardsInPlace board
    playingPlayerCardsSpots :: [CardSpot] =
      [c | (pSpot, c, m) <- allInPlace, pSpot == playingPlayerSpot, isJust m]
    emptyPlayingPlayerSpot =
      cSpot `notElem` playingPlayerCardsSpots && pSpot == playingPlayerSpot

noDrag :: Attribute Action
noDrag = style_ (Map.fromList [("-webkit-user-drag", "none"), ("user-select", "none")])

cardPositionStyle ::
  -- | The horizontal offset from the enclosing container, in number of cells
  Int ->
  -- | The vertical offset from the enclosing container, in number of cells
  Int ->
  Attribute a
cardPositionStyle xCellsOffset yCellsOffset =
  style_ $ pltwh Absolute xPixels yPixels cardPixelWidth cardPixelHeight
  where
    xPixels = xCellsOffset * cellPixelSize
    yPixels = yCellsOffset * cellPixelSize

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

deathFadeout :: AttackEffect -> Int -> Int -> [View Action]
deathFadeout ae x y =
  if death ae
    then
      [ nodeHtml
          "style"
          []
          ["@keyframes deathFadeout { from { opacity: 1; } to { opacity: 0; } }"],
        div_
          [style_ sty]
          [ img_
              [ src_ $ assetsPath assetFilenameSkull,
                width_ $ ms imgw,
                height_ $ ms imgh,
                noDrag
              ]
          ]
      ]
    else []
  where
    sty =
      Map.union
        ( Map.fromList
            [ ("animation-duration", "1s"),
              ("animation-name", "deathFadeout"),
              ("animation-iteration-count", "1"),
              ("animation-timing-function", "ease"),
              ("animation-fill-mode", "forwards")
            ]
        )
        $ pltwh Absolute left top imgw imgh
    (imgw, imgh) :: (Int, Int) = (cellPixelSize, imgw)
    left = (cardPixelWidth - imgw) `div` 2
    top = (cardPixelHeight - imgh) `div` 2

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
zprbwh z pos left top width height =
  Map.fromList
    [ ("z-index", ms z),
      ("position", ms $ show pos),
      ("right", ms top <> "px"),
      ("bottom", ms left <> "px"),
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
