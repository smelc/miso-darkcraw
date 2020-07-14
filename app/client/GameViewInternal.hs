{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This module contains things used in GameView.hs
-- but which are likely rarely modified, to avoid
-- GameView.hs growing too much. Other modules besides
-- 'GameView' should nod depend on this module.
-- |
module GameViewInternal
  ( borderWidth,
    cardBoxShadowStyle,
    cardPositionStyle,
    cardPositionStyle',
    deathFadeout,
    errView,
    keyframes,
    noDrag,
    stackView,
    turnView
  )
where

import Board
import Card
import Constants
import Control.Lens
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Event
import Game (enemySpots)
import Miso hiding (at)
import Miso.String hiding (length)
import Model
import Turn (turnToInt, turnToPlayerSpot)
import Update
import Utils (style1_)
import ViewInternal

errView :: GameModel -> Int -> [View Action]
errView model@GameModel {interaction} z =
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

turnView :: GameModel -> Int -> View Action
turnView model@GameModel {turn} z =
  div_ [style_ turnViewStyle, textStyle] [line1, line2, line3]
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
        $ keyframedImg
          ("24x24_" <> playerImgY <> "_2.png")
          (line2ImgSize, line2ImgSize)
          "box-shadow: 0 0 0 0 rgba(0,255,0,1);"
          ("box-shadow: 0 0 0 " <> ms borderSize <> "px rgba(0,255,0,1);")
          Map.empty
          ("pulse", "infinite", "ease-in-out")
          (Just "alternate")
          Nothing
    line3 :: View Action =
      button_
        [topMarginAttr, onClick EndTurn, buttonStyle]
        [div_ [textStyle] [text "End Turn"]]

-- | The widget showing the number of cards in the stack
stackView :: GameModel -> Int -> View Action
stackView model@GameModel {board, playingPlayer} z =
  button_
    [buttonStyle, positionStyle] -- onClick ShowStack
    [text $ ms stackSize]
  where
    off = cellPixelSize `div` 2
    (width, height) = (cellPixelSize, cellPixelSize)
    positionStyle =
      style_ $
        zprb z Absolute off off
    pLens = spotToLens playingPlayer
    stackSize = board ^. pLens . #stack & length

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

keyframedImg ::
  MisoString ->
  (Int, Int) ->
  MisoString ->
  MisoString ->
  Map.Map MisoString MisoString ->
  (MisoString, MisoString, MisoString) ->
  Maybe MisoString ->
  Maybe MisoString ->
  [View Action]
keyframedImg path (w, h) from to sty (name, iterationCount, timingFunction) direction fillMode =
  [ nodeHtml
      "style"
      []
      [keyframes name from [] to],
    img_
      [ src_ $ assetsPath path,
        width_ $ ms w,
        height_ $ ms h,
        noDrag,
        style_ $
          sty
            & at "animation-duration" ?~ "1s"
            & at "animation-name" ?~ name
            & at "animation-iteration-count" ?~ iterationCount
            & at "animation-timing-function" ?~ timingFunction
            & at "animation-direction" .~ direction
            & at "animation-fill-mode" .~ fillMode
      ]
  ]

-- draw border around some cards if:
-- 1/ card in hand is being hovered or dragged -> draw borders around
--    valid drag targets
-- or 2/ card in place is being hovered -> draw borders around cards
--       be attacked from this card== playingPlayerSpot,
borderWidth :: GameModel -> PlayerSpot -> CardSpot -> Int
borderWidth GameModel {board, interaction, playingPlayer} pSpot cSpot =
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
      [c | (pSpot, c, m) <- allInPlace, pSpot == playingPlayer, isJust m]
    emptyPlayingPlayerSpot =
      cSpot `notElem` playingPlayerCardsSpots && pSpot == playingPlayer

noDrag :: Attribute Action
noDrag = style_ (Map.fromList [("-webkit-user-drag", "none"), ("user-select", "none")])

mainColor :: MisoString
mainColor = "#FFFFFF"

textRawStyle :: [(MisoString, MisoString)]
textRawStyle = [("color", mainColor)]

textStyle :: Attribute action
textStyle = style_ $ Map.fromList textRawStyle

buttonStyle :: Attribute action
buttonStyle =
  style_
    $ Map.fromList
    $ [ ("background-color", "transparent"), -- no background
        ("border", "2px solid " <> mainColor), -- white not-shadowed border
        ("outline", "none") -- don't highlight that it has been pressed
      ]
      ++ textRawStyle

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
      keyframedImg
        assetFilenameSkull
        (imgw, imgh)
        "opacity: 1;"
        "opacity: 0;"
        sty
        ("deathFadeout", "1", "ease")
        Nothing
        $ Just "forwards"
    else []
  where
    sty = pltwh Absolute left top imgw imgh
    (imgw, imgh) :: (Int, Int) = (cellPixelSize, imgw)
    left = (cardPixelWidth - imgw) `div` 2
    top = (cardPixelHeight - imgh) `div` 2
