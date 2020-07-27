{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This module defines the basic building blocks used by views,
-- so that they have a uniform look'n'feel
-- |
module ViewBlocks (ButtonState (..), gui, GUI (..)) where

import Constants (borderSize, greenHTML, yellowHTML)
import Data.List
import qualified Data.Map.Strict as Map
import Miso
import Miso.String
import Miso.Util ((=:))
import ViewInternal

data GUI a = GUI
  { anyButton :: Int -> ButtonState -> ([Attribute a] -> View a) -> View a,
    -- | argument #1 is the z-index
    -- | argument #2 is the button's state
    -- | argument #3 is the style
    -- | argument #4 is the button's text
    textButton :: Int -> ButtonState -> [Attribute a] -> MisoString -> View a
  }

-- Implementation used by the code
gui :: GUI a
gui = _captivatingGUI

-- | Implementation where text buttons have a continuous shrink/grow border
-- | when enabled
_captivatingGUI :: GUI a
_captivatingGUI = GUI {..}
  where
    anyButton z bState f =
      case bState of
        Enabled ->
          keyframed
            (\as -> f (as ++ [buttonStyle bState False]))
            "box-shadow: 0 0 0 0 rgba(0,255,0,1);"
            ("box-shadow: 0 0 0 " <> ms borderSize <> "px rgba(0,255,0,1);")
            ("pulse", "infinite", "ease-in-out")
            (Just "alternate")
            Nothing
        _ -> f [buttonStyle bState True] -- XXX Call _simpleGUI's anyButton
    textButton z bState attrs t =
      anyButton
        z
        bState
        (\as -> button_ (textButtonStyle bState False ++ as ++ attrs) [text t])

-- | Implementation where text buttons have a simple non moving border
_simpleGUI :: GUI a
_simpleGUI = GUI {..}
  where
    anyButton z bState f = f [buttonStyle bState True]
    textButton z bState attrs text =
      button_
        (buttonStyle bState True : attrs)
        [stytextzhv z text 0 0]

disabledHTML :: MisoString
disabledHTML = "#AAAAAA"

buttonStyle :: ButtonState -> Bool -> Attribute a
buttonStyle bState border =
  style_ $
    "background-color" =: "transparent" -- no background
      <> "outline" =: (px borderSize' <> " solid " <> borderColor)
  where
    borderColor =
      case bState of
        Disabled -> disabledHTML
        Enabled -> greenHTML
        Selected -> yellowHTML
    borderSize' = if border then borderSize else 0

textButtonStyle :: ButtonState -> Bool -> [Attribute a]
textButtonStyle bState border =
  [buttonStyle bState border, style_ $ Map.fromList textRawStyle]

data ButtonState
  = -- | Button is disabled
    Disabled
  | -- | Button is enabled
    Enabled
  | -- | Button was enabled and has been chosen
    Selected
