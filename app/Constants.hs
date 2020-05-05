{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Constants where

import Miso
import Miso.String

assetsDir :: String
assetsDir = "assets"

assetsPath :: String -> MisoString
assetsPath filename = ms assetsDir <> "/" <> ms filename

-- | The board's width, in pixels. TODO read it from disk.
boardWidth :: Int
boardWidth = 408

-- | The board's height , in pixels TODO read it from disk.
boardHeight :: Int
boardHeight = 624