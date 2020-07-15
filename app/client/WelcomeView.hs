{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module to display the initial view of the game, i.e. the one
-- that shows up when the game starts.
-- |
module WelcomeView where

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
  div_ [style_ style, style_ flexStyle] [titleDiv]
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
    titleDiv = div_ [style_ titleStyle] [text gameTitle]
    titleFontSize = cellPixelSize + (cellPixelSize `div` 2)
    titleStyle =
      Map.union
        (Map.fromList textRawStyle)
        $ "font-size" =: (ms titleFontSize <> "px")
          <> "z-index" =: ms zpp
          <> "margin-top" =: (ms cellPixelSize <> "px")
