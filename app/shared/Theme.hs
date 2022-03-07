{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module containing themes
module Theme
  ( kindToTheme,
    Kind (..),
    Theme (..),
  )
where

import Data.Maybe (listToMaybe)
import Miso.String (MisoString)

-- | Identifiers of themes
data Kind
  = DarkForest
  | Forest
  deriving (Enum, Bounded, Show)

-- | The names of assets of this theme. Only filenames: no paths.
data Theme = Theme
  { board :: MisoString,
    hand :: MisoString,
    turn :: MisoString
  }
  deriving (Eq)

instance Show Theme where
  show t =
    case listToMaybe $ filter (\k -> kindToTheme k == t) $ [minBound ..] of
      Just kind -> show kind
      Nothing -> "Unknow Theme"

-- | Transforms an identifier to useful data
kindToTheme :: Kind -> Theme
kindToTheme =
  \case
    DarkForest ->
      Theme
        { board = "dark-forest.png",
          hand = "dark-forest-hand.png",
          turn = "turn-dark-forest.png"
        }
    Forest ->
      Theme
        { board = "forest.png",
          hand = "forest-hand.png",
          turn = "turn.png"
        }
