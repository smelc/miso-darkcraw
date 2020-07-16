{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module to display the initial view of the game, i.e. the one
-- that shows up when the game starts.
-- |
module WelcomeView (viewWelcomeModel) where

import Constants
import qualified Data.Map.Strict as Map
import Miso
import Miso.String
import Miso.Util ((=:))
import Model (WelcomeModel (..))
import Update
import ViewInternal

-- | Constructs a virtual DOM from a welcome model
viewWelcomeModel :: WelcomeModel -> View Action
viewWelcomeModel _ =
  div_
    [style_ style]
    [torchesDiv zpp, div_ [style_ flexStyle] [titleDiv, buttonDiv]]
  where
    (z, zpp) = (0, z + 1)
    style =
      Map.union (zpltwh z Relative 0 0 welcomePixelWidth welcomePixelHeight) $
        Map.fromList
          [("background-image", assetsUrl "welcome.png")]
    flexStyle =
      "display" =: "flex"
        <> "flex-direction" =: "column"
        <> "align-items" =: "center"
    textStyle = Map.fromList textRawStyle
    titleDiv = div_ [style_ titleStyle] [text gameTitle]
    titleFontSize = cellPixelSize + (cellPixelSize `div` 2)
    titleStyle =
      textStyle
        <> "font-size" =: (ms titleFontSize <> "px")
        <> "z-index" =: ms zpp
        <> "margin-top" =: (ms cellPixelSize <> "px")
    topMarginAttr = style_ $ "margin-top" =: (ms titleFontSize <> "px")
    buttonDiv =
      button_
        [ style_ $ "margin-top" =: (ms (titleFontSize * 2) <> "px"),
          onClick $ WelcomeAction' WelcomeStart,
          buttonStyle
        ]
        [div_ [style_ textStyle] [text "Start"]]

torchesDiv :: Int -> View Action
torchesDiv z = div_ [] [torch 1, torch $ -1]
  where
    torch x = div_ [style_ $ style x] []
    style x =
      Map.union (tilerb z Absolute 0 (14 + x)) $
        Map.fromList
          [("background-image", assetsUrl "24x24_0_3.png")]
