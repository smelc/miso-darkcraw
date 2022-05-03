{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This module defines the basic building blocks used by views,
-- so that they have a uniform look'n'feel
-- |
module ViewBlocks (ButtonState (..), dummyOn, gui, GUI (..)) where

import qualified Color
import qualified Constants
import qualified Data.Map.Strict as Map
import Miso
import Miso.String
import qualified Nat
import Update (Action (NoOp))
import ViewInternal

data GUI a = GUI
  { anyButton :: Int -> ButtonState -> ([Attribute a] -> View a) -> Styled (View a),
    -- | argument #1 is the z-index
    -- | argument #2 is the button's state
    -- | argument #3 is the style
    -- | argument #4 is the button's text
    textButton :: Int -> ButtonState -> [Attribute a] -> MisoString -> Styled (View a)
  }

-- Implementation used by the code
gui :: GUI a
gui = _captivatingGUI

-- | Implementation where text buttons have a continuous shrink/grow border
-- | when enabled
_captivatingGUI :: GUI a
_captivatingGUI = GUI {..}
  where
    anyButton _ bState f =
      case bState of
        Enabled ->
          keyframed
            (\as -> f (as ++ [buttonStyle bState False]))
            ( keyframes
                (animDataName animData)
                "box-shadow: 0 0 0 0 rgba(0,255,0,1);"
                []
                ("box-shadow: 0 0 0 " <> ms (Nat.natToInt Constants.borderSize) <> "px rgba(0,255,0,1);")
            )
            animData
        _ -> return $ f [buttonStyle bState True] -- XXX Call _simpleGUI's anyButton
      where
        animData =
          (animationData "pulse" "1s" "ease-in-out")
            { animDataDirection = Just "alternate",
              animDataIterationCount = Just "infinite"
            }
    textButton z bState attrs t =
      anyButton
        z
        bState
        (\as -> button_ (textButtonStyle bState False ++ as ++ attrs) [text t])

-- | Implementation where text buttons have a simple non moving border
_simpleGUI :: GUI a
_simpleGUI = GUI {..}
  where
    anyButton _ bState f = return $ f [buttonStyle bState True]
    textButton z bState attrs text =
      return $
        button_
          (buttonStyle bState True : attrs)
          [stytextzhv z text 0 0]

-- | Dummy 'onWithOptions' instance.
-- See https://github.com/dmjio/miso/issues/478
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

buttonStyle :: ButtonState -> Bool -> Attribute a
buttonStyle bState border =
  style_ $
    "background-color" =: "transparent" -- no background
      <> "outline" =: (px borderSize' <> " solid " <> borderColor)
      <> "border" =: "none"
  where
    borderColor =
      Color.html $ case bState of
        Disabled -> Color.disabled
        Enabled -> Color.green
        Selected -> Color.yellow
    borderSize' = if border then (Nat.natToInt Constants.borderSize) else 0

textButtonStyle :: ButtonState -> Bool -> [Attribute a]
textButtonStyle bState border =
  [buttonStyle bState border, style_ $ Map.fromList textRawStyle]

-- | The state of a button
data ButtonState
  = -- | Button is disabled
    Disabled
  | -- | Button is enabled
    Enabled
  | -- | Button was enabled and has been chosen
    Selected
