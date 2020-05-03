{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module View where

import Data.Map.Strict as Map
import Miso
import Miso.String
import Model

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ [] -- [
  [text (ms x), cardBgCell, cardBgCell2, text (ms x)]
  -- $ (Prelude.replicate 3 cardBgCell ++ [text (ms x)])
  -- , Prelude.replicate 3 cardBgCell
  -- , Prelude.replicate 3 cardBgCell
   -- button_ [ onClick AddOne ] [ text "+" ]
 -- , text (ms x)
 -- , button_ [ onClick SubtractOne ] [ text "-" ]
 -- ]

cardBgCell :: View Action =
  button_ [style_ asset] [ text "-" ]
  where asset = cell24 0 2 -- 24x24_0_2.png

cardBgCell2 :: View Action =
  img_ [width_ "24", height_ "24", src_ "assets/24x24_0_2.png"]
  where asset = cell24 0 2 -- 24x24_0_2.png

cell24 :: Int -> Int -> Map.Map MisoString MisoString
cell24 x y =
  cellBackgroundStyle 24 ("24x24_" <> x <> "_" <> y <> ".png")
  where
    x = ms $ show x
    y = ms $ show y

cellBackgroundStyle :: Int -> MisoString -> Map.Map MisoString MisoString
cellBackgroundStyle size asset = 
  let sizeStr :: MisoString = ms $ show size in
  Map.fromList [ ("display", "block")
               , ("width", sizeStr)
               , ("height", sizeStr)
               , ("background-color", "transparent")
               , ("background-image", "url(assets/24x24_0_2.png)")
             ]

imgBackgroundStyle :: Int -> MisoString -> Map.Map MisoString MisoString
imgBackgroundStyle size asset = 
  let sizeStr :: MisoString = ms $ show size in
  Map.fromList [ ("width", sizeStr)
               , ("height", sizeStr)
               , ("src", "url(assets/24x24_0_2.png)")
             ]
