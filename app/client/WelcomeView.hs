{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module to display the initial view of the game, i.e. the one
-- that shows up when the game starts.
-- |
module WelcomeView where

import Constants (assetsUrl, welcomePixelHeight, welcomePixelWidth)
import qualified Data.Map.Strict as Map
import Miso
import Miso.String
import Model (WelcomeModel (..))
import Update
import ViewInternal

-- | Constructs a virtual DOM from a welcome model
viewWelcomeModel :: WelcomeModel -> View Action
viewWelcomeModel _ =
  div_ [style_ style] []
  where
    (z, zpp) = (0, z + 1)
    style =
      Map.union (zpltwh z Relative 0 0 welcomePixelWidth welcomePixelHeight) $
        Map.fromList
          [("background-image", assetsUrl "welcome.png")]
