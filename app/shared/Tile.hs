{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module regarding graphic tiles. This module is meant to be used
-- qualified.
-- |
module Tile where

import GHC.Generics (Generic)

data Filepath = Filepath
  { root :: String,
    fpX :: Int,
    fpY :: Int
  }
  deriving (Eq, Generic, Ord, Show)

-- | The default 24x24 asset shown when an asset is not found.
-- | This makes 'creatureToFilepath' total.
default24Filepath :: Filepath
default24Filepath = Filepath {root = "24x24", fpX = 2, fpY = 3}

-- | The default 16x16 asset shown when an asset is not found.
-- | This makes 'creatureToFilepath' total.
default16Filepath :: Filepath
default16Filepath = Filepath {root = "16x16", fpX = 2, fpY = 1}

filepathToString :: Filepath -> String
filepathToString Filepath {..} =
  root ++ "_" ++ show fpX ++ "_" ++ show fpY ++ ".png"

data Tile
  = BlackAppears0
  | BlackAppears1
  | BlackAppears2
  | BlackAppears3
  | Blood0
  | Blood1
  | Blood2
  | Blood3
  | Bones0
  | Bones1
  | Bones2
  | Bones3
  | Bones4
  | Bones5
  | Bones6
  | Crown
  | CrushingMace
  | DropBlue
  | EvilKnight
  | FlailOfTheDamned
  | GreenPotion
  | Heart
  | HeartBroken
  | HumanArcher
  | HumanChurch
  | HumanGeneral
  | HumanKnight
  | HumanSpearman
  | HumanSwordsman
  | Loupe
  | Ogre
  | SkBanner
  | SkullRedEyes
  | Squire
  | Sword1
  | Sword2
  | Sword3
  | RedPotion
  | UndeadArcher
  | UndeadGhost
  | UndeadMummy
  | UndeadNecromancer
  | UndeadShade
  | UndeadSkeleton
  | UndeadSpecter
  | UndeadVampire
  | UndeadWarrior
  | WhiteAppears0
  | WhiteAppears1
  | WhiteAppears2
  | WhiteAppears3
  | WhiteAppears4
  | ZCaptain
  | ZKing
  | ZKnight
  | ZPriest
  | ZVeteran
  deriving (Eq, Generic, Ord, Show)

-- TODO @smelc rename me to UI
data TileUI = TileUI
  { filepath :: Filepath,
    tile :: Tile
  }
  deriving (Eq, Generic, Show)

data Size
  = -- | Tiles that are 16x16
    Sixteen
  | -- | Tiles that are 24x24
    TwentyFour
