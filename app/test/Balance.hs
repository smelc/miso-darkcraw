{-# LANGUAGE ScopedTypeVariables #-}

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
import Test.Hspec.QuickCheck
import Test.QuickCheck

main :: SharedModel -> SpecWith ()
main shared =
  describe "Balance" $
    prop "Starting team doesn't have an advantage" $
      \(seeds :: [Seeds]) ->
        let shareds = map (\(Seeds ints) -> SharedModel.withSeed shared) seeds in
        let _ =
                 map
                   undefined
                   $ zip allTeams (take nbCivilWars seeds)
         in undefined
  where
    nbCivilWars = 20
    nbTurns = 8

newtype Seeds = Seeds [Int]

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
  (Int, Int)
play shareds t1 t2 nbTurns =
  go shareds
    & map Match.matchResult
    & count (0, 0, 0)
    & (\(w1, _, w2) -> (w1, w2))
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
