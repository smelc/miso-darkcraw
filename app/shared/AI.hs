{-# LANGUAGE DataKinds #-}

-- |
-- This module defines how the AI plays. It is actually just a dispatching
-- module that delegates either to 'MCTSAI', 'HeuristicAI', or 'ConcreteAI'.
-- 'applyDifficulty', 'boardScore', and 'playHand' are exported for tests
module AI (AI.play) where

import Board
import Card
-- import qualified HeuristicAI

import qualified ConcreteAI
import qualified Constants
import qualified Game
import qualified Shared
import qualified Spots
import qualified Turn

-- | Executes the AI.
play ::
  Constants.Difficulty ->
  Shared.Model ->
  Board 'Core ->
  -- | The playing player
  Spots.Player ->
  -- | The current turn
  Turn.Turn ->
  -- | Events generated for player 'pSpot'
  [Game.Place]
play difficulty shared board pSpot turn =
  -- HeuristicAI.play difficulty shared board pSpot
  ConcreteAI.play difficulty shared board turn pSpot

-- MCTSAI.newPlaySim difficulty shared pSpot board
