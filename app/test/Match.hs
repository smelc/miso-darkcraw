{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- This module simulates playing an entire game
-- |
module Match (main) where

import qualified AI (aiPlay)
import Board
import Card
import Data.Function ((&))
import Data.Text (Text)
import qualified Game
import Model
import SharedModel
import Test.Hspec
import Turn
import Update

-- TODO @smelc 1/ implement main
-- 2/ alternative mode that uses the main loop

main :: SpecWith ()
main = undefined

data Result = Draw | Error Text | Win PlayerSpot

play :: SharedModel -> Team -> Team -> Int -> Result
play shared opponent player nbTurns =
  go $ initialGameModel shared opponent player
  where
    go m@GameModel {turn} | turnToInt turn > nbTurns = toResult m
    go m = playOneTurn m & either Error go

toResult :: GameModel -> Result
toResult GameModel {board} =
  if
      | scoreTop == scoreBot -> Draw
      | scoreTop > scoreBot -> Win PlayerTop
      | otherwise -> Win PlayerBottom
  where
    score pSpot = boardToPart board pSpot & Board.score
    (scoreTop, scoreBot) = (score PlayerTop, score PlayerBottom)

playOneTurn :: GameModel -> Either Text GameModel
playOneTurn m = playPlayerTurn m >>= playPlayerTurn

playPlayerTurn :: GameModel -> Either Text GameModel
playPlayerTurn m@GameModel {board, gameShared = shared, turn} = do
  Game.Result board' _ _ <- Game.playAll shared board events
  return $ m {board = board', turn = nextTurn turn}
  where
    events = AI.aiPlay shared board turn
