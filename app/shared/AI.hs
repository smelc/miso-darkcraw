{-# LANGUAGE DataKinds #-}

-- |
-- This module defines how the AI plays. It is actually just a dispatching
-- module that delegates either to 'MCTSAI' or to 'HeuristicAI'.
-- 'applyDifficulty', 'boardScore', and 'playHand' are exported for tests
module AI (AI.play) where

import Board
import Card
import qualified Constants
import qualified Game
import qualified HeuristicAI
import SharedModel (SharedModel)
import qualified Spots

-- | Executes the AI.
play ::
  Constants.Difficulty ->
  SharedModel ->
  Board 'Core ->
  -- | The playing player
  Spots.Player ->
  -- | Events generated for player 'pSpot'
  [Game.Place]
play difficulty shared board pSpot =
  HeuristicAI.play difficulty shared board pSpot

-- MCTSAI.newPlaySim difficulty shared pSpot board
