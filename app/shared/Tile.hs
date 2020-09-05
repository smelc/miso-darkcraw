{-# LANGUAGE DeriveGeneric #-}

module Tile where

import GHC.Generics (Generic)

data Tile
  = Crown
  | GreenPotion
  | Heart
  | Sword
  | RedPotion
  deriving (Eq, Generic, Show)

data TileUI = TileUI
  { filename :: String,
    tile :: Tile,
    x :: Int,
    y :: Int
  }
  deriving (Eq, Generic, Show)