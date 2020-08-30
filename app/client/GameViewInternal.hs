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
    deathFadeout,
    errView,
    heartWobble,
    keyframes,
    noDrag,
    scoreViews,
    StackType (..),
    stackView,
    turnView,
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
import ViewBlocks (ButtonState (..), gui, textButton)
import ViewInternal

errView :: GameModel -> Int -> [View Action]
errView model@GameModel {interaction} z =
  case interaction of
    GameShowErrorInteraction msg ->
      [div_ [style_ errViewStyle] $ textView msg : feedbackViews]
    _ -> []
  where
    (width, height) = (504, 168)
    left = (boardPixelWidth - width) `div` 2
    top = boardPixelHeight `div` 2
    errViewStyle =
      Map.union (zpltwh z Relative left top width height)
        $ Map.union flexColumnStyle
        $ Map.fromList
          [ ("justify-content", "center"),
            ("background-image", assetsUrl "errbox.png")
          ]
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

scoreViews :: GameModel -> Int -> [View Action]
scoreViews m@GameModel {board} z =
  [ scoreView m z PlayerTop,
    scoreView m z PlayerBottom
  ]
    ++ [ scoreLeaderView
           (if topScore > botScore then PlayerTop else PlayerBottom)
         | topScore /= botScore
       ]
  where
    topScore = board ^. #playerTop . #score
    botScore = board ^. #playerBottom . #score

scoreMarginTop :: PlayerSpot -> Int
scoreMarginTop PlayerTop = cps
scoreMarginTop PlayerBottom = cps * 24

scoreView :: GameModel -> Int -> PlayerSpot -> View Action
scoreView GameModel {board} z pSpot =
  div_
    [ style_ $
        Map.fromList textRawStyle
          <> flexColumnStyle
          <> "z-index" =: ms z
          <> "position" =: "absolute"
          -- Center horizontally
          <> "margin-left" =: "50%"
          <> "margin-right" =: "50%"
          -- And tell the element to center horizontally, not to its left
          <> "transform" =: "translate(-50%, 0%)"
          -- Finally shift element down
          <> "margin-top" =: px (scoreMarginTop pSpot)
    ]
    [ div_ [] [text "Score"],
      div_ [] [text $ ms $ show score]
    ]
  where
    score = board ^. (spotToLens pSpot . #score)

-- Not in scoreView itself, otherwise it mixes up the central alignment
-- (that uses scoreView's div own width (50%))
scoreLeaderView :: PlayerSpot -> View Action
scoreLeaderView pSpot =
  img_
    [ src_ (assetsPath assetFilenameCrown),
      style_ $
        "margin-top" =: px (scoreMarginTop pSpot + cps `div` 2)
          <> "margin-left" =: px ((cps * 11) + cps `div` 2)
    ]

turnView :: GameModel -> Int -> Styled (View Action)
turnView model@GameModel {turn} z = do
  line3 <- line3M
  return $ div_ [style_ $ turnViewStyle <> textStyle] [line1, line2, line3]
  where
    turnViewStyle =
      zprbwh z Absolute 0 0 turnPixelWidth turnPixelHeight
        <> flexColumnStyle
        <> "justify-content" =: "center"
        <> "background-image" =: assetsUrl "turn.png"
    line1 :: View Action = text $ "Turn " <> ms (turnToInt turn)
    playerImgY =
      case turnToPlayerSpot turn of
        PlayerTop -> "1"
        PlayerBottom -> "2"
    topMargin = cellPixelSize `div` 2
    topMarginAttr = style_ $ "margin-top" =: px topMargin
    line2 :: View Action =
      div_
        [topMarginAttr]
        [img_ [src_ (assetsPath $ "24x24_" <> playerImgY <> "_2.png")]]
    line3M :: Styled (View Action) =
      textButton
        gui
        z
        (if isPlayerTurn model then Enabled else Disabled)
        [ topMarginAttr,
          onClick $ GameAction' GameEndTurnPressed
        ]
        "End Turn"

data StackType
  = Stacked
  | Discarded

-- | The widget showing the number of cards in the stack/discarded stack
stackView :: GameModel -> StackType -> Int -> Styled (View Action)
stackView m@GameModel {board, playingPlayer} stackType z = do
  button <- textButton gui z Enabled [] $ ms (label ++ ": " ++ show stackSize)
  return $ div_ [positionStyle, onClick $ DeckGo deck] [button]
  where
    xoff = cps * 4
    yoff = cps `div` 2
    positionStyle =
      style_ $
        "z-index" =: ms z <> "position" =: "absolute"
          <> marginSide =: px xoff
          <> "bottom" =: px yoff
    pLens = spotToLens playingPlayer
    (getter, label, marginSide) = case stackType of
      Stacked -> (#stack, "Stack", "right")
      Discarded -> (#discarded, "Discarded", "left")
    deck = board ^. pLens . getter
    stackSize = length deck

-- draw border around some cards if:
-- 1/ card in hand is being hovered or dragged -> draw borders around
--    valid drag targets
-- or 2/ card in place is being hovered -> draw borders around cards
--       be attacked from this card== playingPlayerSpot,
borderWidth :: GameModel -> PlayerSpot -> CardSpot -> Int
borderWidth GameModel {board, interaction, playingPlayer} pSpot cSpot =
  case interaction of
    GameDragInteraction _ | emptyPlayingPlayerSpot -> 3
    GameHoverInteraction _ | emptyPlayingPlayerSpot -> 3
    GameHoverInPlaceInteraction pSpot' cSpotHovered ->
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

deathFadeout :: AttackEffect -> Int -> Int -> Styled [View Action]
deathFadeout ae x y =
  sequence
    [ keyframed
        builder
        (keyframes (animDataName animData) "opacity: 1;" [] "opacity: 0;")
        animData
      | death ae
    ]
  where
    sty = pltwh Absolute left top imgw imgh
    (imgw, imgh) :: (Int, Int) = (cellPixelSize, imgw)
    left = (cardPixelWidth - imgw) `div` 2
    top = (cardPixelHeight - imgh) `div` 2
    builder x =
      img_ $ [src_ (assetsPath assetFilenameSkull), style_ sty] ++ x
    animData =
      (animationData "deathFadeout" "1s" "ease")
        { animDataFillMode = Just "forwards"
        }

heartWobble :: Int -> AttackEffect -> Int -> Int -> Styled [View Action]
heartWobble z ae x y =
  sequence
    [ keyframed
        builder
        (wobblev (animDataName animData) True imgw (imgh * 3))
        animData
      | delay <- delays,
        let animData = createAnimData delay
    ]
  where
    hpc = hitPointsChange ae
    hpLoss = hpc < 0
    delay = 250 -- The delay between each wobbling heart, milliseconds
    delays =
      [delay * (i - 1) | i <- if hpLoss then [1 .. (- hpc)] else []]
    sty = pltwh Absolute left top imgw imgh <> Map.singleton "z-index" (ms z)
    (imgw, imgh) :: (Int, Int) = (seize, imgw)
    left = (cardPixelWidth - imgw) `div` 2
    top = 0
    builder x =
      img_ $ [src_ (assetsPath assetFilenameHeart), style_ sty] ++ x
    createAnimData delay =
      (animationData "heartWobble" "1s" "linear")
        { animDataFillMode = Just "forwards",
          animDataDelay = Just $ ms delay <> "ms"
        }
