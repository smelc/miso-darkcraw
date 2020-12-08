{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- This module simulates playing an entire game
-- |
-- play is exported for debugging with ghci
module Match (main, play) where

import qualified AI (aiPlay)
import Board
import Card
import Data.Function ((&))
import Data.Text (Text)
import Debug.Trace (traceShow)
import qualified Game
import Generators ()
import Model
import SharedModel
import Test.Hspec
import Test.Hspec.QuickCheck
import Turn
import Update

-- TODO @smelc: alternative mode that uses the main loop

main :: SharedModel -> SpecWith ()
main shared =
  describe "Playing a match doesn't return Error" $
    prop "forall t1, t2 :: Team, play shared t1 t2 8 isn't Error" $
      \(t1, t2) -> not $ isError . matchResult $ play shared t1 t2 8
  where
    isError (Error msg) = traceShow msg True
    isError Draw = False
    isError (Win _) = False

data MatchResult = Draw | Error Text | Win PlayerSpot
  deriving (Show)

data Result = Result
  { boards :: [Board 'Core],
    matchResult :: MatchResult
  }
  deriving (Show)

play :: SharedModel -> Team -> Team -> Int -> Result
play shared opponent player nbTurns =
  go (initialGameModel shared opponent player) []
  where
    go m@GameModel {turn} boards
      | turnToInt turn > nbTurns =
        Result (reverse boards) $ toMatchResult m
    go m boards =
      case playOneTurn m of
        Left msg -> Result boards $ Error msg
        Right m'@GameModel {board} -> go m' $ board : boards

toMatchResult :: GameModel -> MatchResult
toMatchResult GameModel {board} =
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
