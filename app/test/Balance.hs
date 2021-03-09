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
      \(seedsList :: [Seeds]) ->
        let shareds = map (\(Seeds seeds) -> map (SharedModel.withSeed shared) seeds) seedsList
         in let teamNShareds = zip allTeams shareds
             in let results = map (\(t, shareds) -> (t, play (take nbCivilWars shareds) t t nbTurns)) teamNShareds
                 in results `shouldSatisfy` civilWarsSpec
  where
    nbCivilWars = 20 -- Number of games for endomatches
    nbTurns = 8
    civilWarsSpec :: [(Team, (Int, Int))] -> Bool
    civilWarsSpec [] = True
    civilWarsSpec _ = False -- TODO

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
