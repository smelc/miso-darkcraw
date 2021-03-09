{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This module tests the balance
-- |
module Balance where

import Board
import Card
import Data.Function ((&))
import Debug.Trace
import GHC.Float
import qualified Match
import SharedModel (SharedModel)
import qualified SharedModel
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

main :: SharedModel -> SpecWith ()
main shared =
  describe "Balance" $
    prop "Starting team doesn't have an advantage" $
      \(Seeds seeds :: Seeds) ->
        let shareds = take nbCivilWars seeds & map (SharedModel.withSeed shared)
         in let results = map (\t -> (t, play shareds t t nbTurns)) allTeams
             in results `shouldSatisfy` civilWarsSpec
  where
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
        _ -> civilWarsSpec rest
      where
        (nbWins, win1f) = (int2Float $ win1 + win2, int2Float win1)
        (min, max) = (nbWins * 0.4, nbWins * 0.6)

newtype Seeds = Seeds [Int]
  deriving (Show)

instance Arbitrary Seeds where
  arbitrary = Seeds <$> infiniteList

play ::
  -- | The models to use for a start (length of this list implies the
  -- the number of games to play)
  [SharedModel] ->
  Team ->
  Team ->
  -- | The number of turns to play
  Int ->
  -- | The number of wins of the first team, the number of draws, and
  -- the number of wins of the second team
  (Int, Int, Int)
play shareds t1 t2 nbTurns =
  go shareds
    & map Match.matchResult
    & count (0, 0, 0)
  where
    go (shared : rest) =
      Match.play (mkGameModel shared) nbTurns : go rest
    go [] = []
    count acc [] = acc
    count (w1, d, w2) (Match.Draw : tail) = count (w1, d + 1, w2) tail
    count tuple (Match.Error {} : tail) = count tuple tail -- not this test's business to fail on Error
    count (w1, d, w2) (Match.Win PlayerTop : tail) = count (w1, d, w2 + 1) tail
    count (w1, d, w2) (Match.Win PlayerBot : tail) = count (w1 + 1, d, w2) tail
    mkGameModel = undefined
