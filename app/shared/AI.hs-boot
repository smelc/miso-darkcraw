{-# LANGUAGE DataKinds #-}

-- | Subset of AI used by 'Move', to break the cyclic dependency between
-- 'Move' and 'AI'.
module AI where

import Board
import Card
import Constants (Difficulty)
import qualified Game
import SharedModel (SharedModel)
import qualified Spots

play ::
  Difficulty ->
  SharedModel ->
  Board 'Core ->
  -- | The playing player
  Spots.Player ->
  -- | Events generated for player 'pSpot'
  [Game.Place]
