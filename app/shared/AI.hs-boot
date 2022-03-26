{-# LANGUAGE DataKinds #-}

-- | Subset of AI used by 'Move', to break the cyclic dependency between
-- 'Move' and 'AI'.
module AI where

import Board
import Card
import Constants (Difficulty)
import qualified Game
import qualified Shared
import qualified Spots
import qualified Turn

play ::
  Difficulty ->
  Shared.Model ->
  Board 'Core ->
  -- | The playing player
  Spots.Player ->
  Turn.Turn ->
  -- | Events generated for player 'pSpot'
  [Game.Place]
