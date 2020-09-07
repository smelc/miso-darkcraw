{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Takes instances of scene diffs from 'Movie' and makes DOM trees
-- out of that
module Projector where

import Card
import Cinema (Change, Element (..), State (..))
import Constants (assetsPath)
import Data.Function ((&))
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust)
import Miso
import Miso.String hiding (find, map)
import SharedModel (SharedModel (..))
import ViewInternal (imgCell)

data Context = Context
  { z :: Int,
    paths :: Map.Map CreatureID MisoString
  }

createContext :: Int -> SharedModel -> Context
createContext z SharedModel {..} =
  Context {..}
  where
    paths = map f sharedCards & catMaybes & Map.fromList
    f (CreatureCard Creature {..}) = Just (creatureId, ms filename)
    f _ = Nothing

viewEntry :: Context -> Element -> Change -> View a
viewEntry Context {..} element change =
  case element of
    Actor _ cid -> imgCell $ assetsPath $ paths Map.!? cid & fromJust
    Tile -> error "Please implement viewing a Tile"
