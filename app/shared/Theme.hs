{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module containing themes
module Theme where

import Miso.String (MisoString)

data Kind
  = DarkForest
  | Forest

data Theme = Theme
  { board :: MisoString,
    hand :: MisoString,
    turn :: MisoString
  }

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
