{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Takes instances of scene diffs from 'Movie' and makes DOM trees
-- out of that
module Projector (Shooting (..), viewMovie) where

import Card
import Cinema (Change, Element (..), Phase (..), Scene, State (..), initial, patch)
import Constants (assetsPath)
import Data.Function ((&))
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust)
import Miso
import Miso.String hiding (find, map)
import SharedModel (SharedModel (..))
import ViewInternal (imgCell)

data Shooting = Shooting
  { scene :: Maybe (Scene Display),
    rest :: [Scene Diff]
  }

viewMovie ::
  SharedModel ->
  -- | The previous scene that was returned, or Nothing if the first call
  Maybe (Scene Display) ->
  -- | The remaing scenes to apply
  [Scene Diff] ->
  Shooting
viewMovie _ _ [] = Shooting {scene = Nothing, rest = []}
viewMovie shared prev (diff : rest) =
  let scene = Just $
        case prev of
          -- Building the first scene, 'diff' gets interpreted
          -- as absolute, not a diff.
          Nothing -> initial diff
          -- Patching previous scene
          Just prev -> patch prev diff
   in Shooting {..}

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
