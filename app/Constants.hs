{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Constants where

import Miso.String

assetsDir :: MisoString
assetsDir = "assets"

assetFilenameBeigeBG :: MisoString
assetFilenameBeigeBG = "24x24_0_2.png"

assetFilenameHumanGeneral :: MisoString
assetFilenameHumanGeneral = "24x24_3_0.png"

assetFilenameHumanSpearman :: MisoString
assetFilenameHumanSpearman = "24x24_0_0.png"

assetFilenameHeart :: MisoString
assetFilenameHeart = "16x16_0_0.png"

assetFilenameSword :: MisoString
assetFilenameSword = "16x16_1_0.png"

assetsPath :: MisoString -> MisoString
assetsPath filename = assetsDir <> "/" <> filename

-- | The number of cells from the left of the board to leftmost cards
boardToLeftCardCellsOffset :: Int
boardToLeftCardCellsOffset = 5

-- | The board's width, in pixels
boardPixelWidth :: Int
boardPixelWidth = 504

-- | The board's height , in pixels
boardPixelHeight :: Int
boardPixelHeight = 624

-- | The hands's height, in pixels
handPixelHeight :: Int
handPixelHeight = 192

-- | The hands's width, in pixels
handPixelWidth :: Int
handPixelWidth = boardPixelWidth

-- | A card's height, in cells
cardCellHeight :: Int
cardCellHeight = 4

-- | A card's width, in cells
cardCellWidth :: Int
cardCellWidth = 3

-- | A card's height, in pixels
cardPixelHeight :: Int
cardPixelHeight = cellPixelSize * cardCellHeight

-- | A card's width, in pixels
cardPixelWidth :: Int
cardPixelWidth = cellPixelSize * cardCellWidth

-- | The horizontal spacing between two cards, in cells
cardHCellGap :: Int
cardHCellGap = 1

-- | The vertical spacing between two cards, in cells
cardVCellGap :: Int
cardVCellGap = 1

-- | The vertical spacing between the two teams, in cells
teamsVCellGap :: Int
teamsVCellGap = 2

-- | The size of a cell, in pixels
cellPixelSize :: Int
cellPixelSize = 24
