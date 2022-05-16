{-# LANGUAGE OverloadedStrings #-}

-- | Module displaying the world map, which is the first view
-- in legendary edition as well as the view between levels.
module WorldView where

import qualified Constants
import Miso
import qualified Model
import qualified Update
import ViewInternal (Position (..), Styled, px, zpltwh)

viewWorldModel :: Model.World -> Styled (View Update.Action)
viewWorldModel _ =
  return $
    div_
      [style_ bgStyle]
      []
  where
    bgStyle =
      zpltwh 0 Relative 0 0 Constants.lobbiesPixelWidth worldViewPixelHeight
        <> "background-image" =: Constants.assetsUrl "world.png"
        <> "background-position-x" =: px (-(Constants.cps * 13))
        <> "background-position-y" =: px worldViewPixelHeight

worldViewPixelHeight :: Int
worldViewPixelHeight = Constants.lobbiesPixelHeight
