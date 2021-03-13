{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This module tests the balance
-- |
module Balance where

import Board
import Card
import Data.Function ((&))
import qualified Data.Text as Text
import Debug.Trace
import GHC.Float
import qualified Match
import Model (GameModel (..))
import SharedModel (SharedModel)
import qualified SharedModel
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Update

main :: SharedModel -> SpecWith ()
main shared =
  describe "Balance" $
    prop "Starting team doesn't have an advantage" $
      let shareds = take nbCivilWars seeds & map (SharedModel.withSeed shared)
       in let results = map (\t -> (t, Balance.play shareds (Teams t t) nbTurns)) allTeams
           in results `shouldSatisfy` civilWarsSpec
  where
    seeds = [0, 31 ..]
    nbCivilWars = 20 -- Number of games for endomatches
    nbTurns = 8
    civilWarsSpec :: [(Team, (Int, Int, Int))] -> Bool
    civilWarsSpec [] = True
    civilWarsSpec ((t, (win1, draws, win2)) : rest) =
      case (min <= win1f, win1f <= max) of
        (False, _) ->
          traceShow
            (show t ++ " VS " ++ show t ++ ": not enough wins: " ++ show win1 ++ ", expected at least " ++ show min)
            False
        (_, False) ->
          traceShow
            (show t ++ " VS " ++ show t ++ ": too many wins: " ++ show win1 ++ ", expected at most " ++ show max)
            False
        _ ->
          traceShow
            (show min ++ " <= " ++ show win1f ++ " <= " ++ show max)
            $ civilWarsSpec rest
      where
        (nbWins, win1f) = (int2Float $ win1 + win2, int2Float win1)
        (min, max) = (nbWins * 0.4, nbWins * 0.6)

play ::
  -- | The models to use for a start (length of this list implies the
  -- the number of games to play)
  [SharedModel] ->
  Teams ->
  -- | The number of turns to play
  Int ->
  -- | The number of wins of the first team, the number of draws, and
  -- the number of wins of the second team
  (Int, Int, Int)
play shareds teams nbTurns =
  go shareds
    & map Match.matchResult
    & count (0, 0, 0)
  where
    go (shared : rest) =
      let result = Match.play (Update.initialGameModel shared teams) nbTurns
       in traceShow (logString result) result : go rest
    go [] = []
    count acc [] = acc
    count (w1, d, w2) (Match.Draw : tail) = count (w1, d + 1, w2) tail
    count tuple (Match.Error {} : tail) = count tuple tail -- not this test's business to fail on Error
    count (w1, d, w2) (Match.Win PlayerTop : tail) = count (w1, d, w2 + 1) tail
    count (w1, d, w2) (Match.Win PlayerBot : tail) = count (w1 + 1, d, w2) tail
    logString Match.Result {Match.models, Match.matchResult} =
      case matchResult of
        Match.Draw -> "Draw " ++ show (team PlayerTop) ++ " VS " ++ show (team PlayerBot)
        Match.Error err -> "[ERR] " ++ Text.unpack err
        Match.Win pSpot -> winLabel pSpot PlayerTop ++ " VS " ++ winLabel pSpot PlayerBot
      where
        lastBoard = last models & board
        team pSpot = boardToPart lastBoard pSpot & Board.team
        winLabel winner pSpot =
          case winner == pSpot of
            True -> "Win  " ++ show (team pSpot)
            False -> "Lost " ++ show (team pSpot)
