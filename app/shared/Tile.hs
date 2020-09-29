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
  | Crown
  | GreenPotion
  | Heart
  | Sword
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
