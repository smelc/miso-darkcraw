{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Constants where

import Miso
import Miso.String

assetsDir :: MisoString
assetsDir = "assets"

assetFilenameBeigeBG :: MisoString
assetFilenameBeigeBG = "24x24_0_2.png"

assetFilenameHumanGeneral :: MisoString
assetFilenameHumanGeneral = "24x24_3_0.png"

assetFilenameHumanSpearman :: MisoString
assetFilenameHumanSpearman = "24x24_0_0.png"

assetFilenameHeart:: MisoString
assetFilenameHeart = "16x16_0_0.png"

assetFilenameSword :: MisoString
assetFilenameSword = "16x16_1_0.png"

assetsPath :: MisoString -> MisoString
assetsPath filename = assetsDir <> "/" <> filename

-- | The board's width, in pixels. TODO read it from disk.
boardWidth :: Int
boardWidth = 408

-- | The board's height , in pixels TODO read it from disk.
boardHeight :: Int
boardHeight = 624

cardHeight :: Int
cardHeight = cellSize * 4

cardWidth :: Int
cardWidth = cellSize * 3

cellSize :: Int
cellSize = 24
