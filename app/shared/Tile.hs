{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module regarding graphic tiles. This module is meant to be used
-- qualified.
-- |
module Tile where

import GHC.Generics (Generic)
import Nat

data Filepath = Filepath
  { root :: String,
    fpX :: Int,
    fpY :: Int
  }
  deriving (Eq, Generic, Ord, Show)

-- FIXME @smelc write instance MisoString.toMisoString Filepath

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
  = Abomination
  | Arrow
  | Assassin
  | AxeOfRage
  | BannerFeather
  | Bear
  | BeastmenDefender
  | BeastmenMinotaur
  | BeastmenSpearman
  | Beholder
  | BirdWhite
  | BlackAppears0
  | BlackAppears1
  | BlackAppears2
  | BlackAppears3
  | Blood0
  | Blood1
  | Blood2
  | Blood3
  | BloodDrop
  | Bones0
  | Bones1
  | Bones2
  | Bones3
  | Bones4
  | Bones5
  | Bones6
  | BowOfGaia
  | BowOfStrength
  | Chest
  | CloakOfGaia
  | Crown
  | CrushingMace
  | Daemon
  | DropBlue
  | EvilKnight
  | EvilTroll
  | EvilPriest
  | EvilSpearman
  | Explosion
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
  | HuntingHorn
  | Loupe
  | Man
  | Ogre
  | Pandemonium
  | RedPotion
  | Shield
  | SkBanner
  | SkullRedEyes
  | SpikyMace
  | Squire
  | StrengthPot
  | Sword1
  | Sword2
  | Sword3
  | SylvanArcher
  | SylvanFalcon
  | SylvanFalconer
  | SylvanGuardian
  | SylvanPriest
  | SylvanSpearwoman
  | SylvanRanger
  | SylvanWorm
  | SwordOfBlood
  | Trebuchet
  | Tree
  | Troll
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
  | Wings
  | ZCaptain
  | ZKing
  | ZKnight
  | ZPriest
  | ZVeteran
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

-- | All tiles
all :: [Tile]
all = [minBound ..]

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

-- | The pixel size of a 'Size' value
sizeToNat :: Size -> Nat
sizeToNat = \case Sixteen -> 16; TwentyFour -> 24
