{-# LANGUAGE OverloadedStrings #-}

-- | Module containing color constants. Module is meant to be used qualified.
module Color where

import Miso.String
import Nat

data T = T
  { -- | The HTML representation like @#FF00FF@
    html :: MisoString,
    -- | The RGB representation
    rgb :: (Nat, Nat, Nat)
  }

beige :: T
beige = T "#F6E705" undefined

disabled :: T
disabled = T "#AAAAAA" undefined

green :: T
green = T "#00FF00" (0, 255, 0)

grey :: T
grey = T "#555555" undefined

-- | T to highlight something being hovered
hover :: T
hover = red

-- | Converts an 'rgb' value to using 'Nat'
rgbToInt :: (Nat, Nat, Nat) -> (Int, Int, Int)
rgbToInt (x, y, z) = (Nat.natToInt x, Nat.natToInt y, Nat.natToInt z)

red :: T
red = T "#FF0000" (255, 0, 0)

-- | Color to highlight something selected
selection :: T
selection = yellow

yellow :: T
yellow = T "#FFFF00" (255, 255, 0)

white :: T
white = T "#FFFFFF" (255, 255, 255)
