-- |
-- This module tests the balance
-- |
module Balance where

import Board
import Card
import Data.Function ((&))
import qualified Match
import SharedModel (SharedModel)
import qualified SharedModel
import Test.Hspec

main :: SharedModel -> SpecWith ()
main shared = undefined

play ::
  SharedModel ->
  Team ->
  Team ->
  -- | A seed
  Int ->
  -- | The number of turns of a game to play
  Int ->
  -- | The number of games to play
  Int ->
  -- | The number of wins of the first team, the number of draws, and
  -- the number of wins of the second team
  (Int, Int)
play shared t1 t2 seed nbTurns nbGames =
  go (SharedModel.withSeed shared seed) nbGames
    & map Match.matchResult
    & count (0, 0, 0)
    & (\(w1, _, w2) -> (w1, w2))
  where
    go s i
      | i > 0 =
        Match.play undefined nbTurns : go (SharedModel.withSeed s $ seed + i) (i - 1)
    go s 0 = []
    go s i = error $ "Unexpected negative integer: " ++ show i
    count acc [] = acc
    count (w1, d, w2) (Match.Draw : tail) = count (w1, d + 1, w2) tail
    count tuple (Match.Error {} : tail) = count tuple tail -- not this test's business to fail on Error
    count (w1, d, w2) (Match.Win PlayerTop : tail) = count (w1, d, w2 + 1) tail
    count (w1, d, w2) (Match.Win PlayerBot : tail) = count (w1 + 1, d, w2) tail
