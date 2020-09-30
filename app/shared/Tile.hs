{-# LANGUAGE DeriveGeneric #-}

module Tile
  ( Tile (..),
    TileUI (..),
  )
where

import Card (Filepath (..))
import GHC.Generics (Generic)

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
  | GreenPotion
  | Heart
  | Sword1
  | Sword2
  | RedPotion
  | WhiteAppears0
  | WhiteAppears1
  | WhiteAppears2
  | WhiteAppears3
  | WhiteAppears4
  deriving (Eq, Generic, Ord, Show)

data TileUI = TileUI
  { filepath :: Filepath,
    tile :: Tile
  }
  deriving (Eq, Generic, Show)
