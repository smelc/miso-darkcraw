{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This module defines the basic building blocks used by views,
-- so that they have a uniform look'n'feel
-- |
module ViewBlocks (gui, GUI (..)) where

import Data.List
import qualified Data.Map.Strict as Map
import Miso
import Miso.String
import Miso.Util ((=:))
import ViewInternal hiding (buttonStyle)

data GUI a = GUI
  { -- | argument #1 is the z-index
    -- | argument #2 is whether the button is enabled
    -- | argument #3 is the style
    -- | argument #4 is the button's text
    textButton :: Int -> Bool -> [Attribute a] -> MisoString -> [View a],
    x :: Int -- Temporary
  }

gui :: GUI a
gui = simpleGUI

simpleGUI :: GUI a
simpleGUI = GUI {..}
  where
    textButton z enabled attrs text =
      [ button_
          (buttonStyle enabled : attrs)
          [stytextzhv z text 0 0]
      ]
    x = 0

disabledColor :: MisoString
disabledColor = "#CCCCCC"

buttonStyle :: Bool -> Attribute a
buttonStyle enabled =
  style_ $
    ("border" =: ("2px solid " <> borderColor))
      <> "background-color" =: "transparent" -- no background
      <> "outline" =: "none" -- don't highlight that it has been pressed
      <> Map.fromList textRawStyle
  where
    borderColor = if enabled then textMainColor else disabledColor
