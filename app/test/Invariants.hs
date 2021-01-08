{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Invariants where

import Board
import Card
import Data.Function ((&))
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Debug.Trace (traceShow)
import qualified Match
import Pretty
import SharedModel
import Test.Hspec
import Test.Hspec.QuickCheck

class Invariant a where
  -- | Violations of the invariant if any, otherwise []
  violation :: a -> [String]

instance Invariant (Creature 'Core) where
  violation Creature {..} =
    ["Creature's attack should be >= 0 but found " ++ show attack | attack < 0]
      ++ ["Creature's hp should be >= 0 but found " ++ show hp | hp < 0]

instance Invariant (PlayerPart 'Core) where
  violation PlayerPart {..} =
    ["Score should be >= 0 but found " ++ show score | score < 0]
      ++ (map violation (Map.elems inPlace) & concat)

instance Invariant (Board 'Core) where
  violation Board {playerTop, playerBottom} =
    violation playerTop ++ violation playerBottom

main :: SharedModel -> SpecWith ()
main shared =
  describe "Board invariant" $ do
    prop "holds initially" $
      \(Pretty teams, seed) ->
        let shared' = SharedModel.withSeed shared seed
         in (initialBoard shared' teams & snd) `shouldSatisfy` isValid
    prop "is preserved by playing matches" $
      \(Pretty team1, Pretty team2, seed) ->
        let shared' = SharedModel.withSeed shared seed
         in (Match.play shared' team1 team2 seed & Match.boards & last)
              `shouldSatisfy` isValid'
  where
    last l = reverse l & listToMaybe
    isValid x =
      case violation x of
        [] -> True
        violations -> traceShow (unlines violations) False
    isValid' Nothing = True
    isValid' (Just b) = isValid b
