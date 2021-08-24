{-# LANGUAGE OverloadedStrings #-}

module LootView where

import qualified Constants
import qualified Data.Map.Strict as Map
import Miso
import Miso.String
import Model (LootModel (..))
import Update
import ViewInternal

-- |
-- Module to display the view reached after a successful end of 'GameView'.
-- This view sits between two games, where the player can pick new
-- cards to augment its deck.
-- |
view :: LootModel -> Styled (View Action)
view LootModel {} = do
  return $
    div_
      [style_ $ bgStyle 0]
      []
  where
    bgStyle :: Int -> Map.Map MisoString MisoString
    bgStyle z =
      zpltwh z Relative 0 0 Constants.lobbiesPixelWidth Constants.boardPixelHeight
        <> "background-image" =: Constants.assetsUrl "loot-forest-sand.png"
