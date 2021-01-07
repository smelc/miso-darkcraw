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
  -- | An explanation why the invariant is violated, otherwise 'Nothing'
  violation :: a -> Maybe String

join :: [Maybe String] -> Maybe String
join [] = Nothing
join (Nothing : rest) = join rest
join (Just violation : rest) =
  case join rest of
    Nothing -> Just violation
    Just violations -> Just $ violation ++ "\n" ++ violations

instance Invariant (Creature 'Core) where
  violation Creature {..}
    | attack < 0 = Just $ "Creature's attack should be >= 0 but found " ++ show attack
    | hp < 0 = Just $ "Creature's hp should be >= 0 but found " ++ show hp
    | otherwise = Nothing

instance Invariant (PlayerPart 'Core) where
  violation PlayerPart {..}
    | score < 0 = Just $ "Score should be >= 0 but found " ++ show score
    | otherwise = map violation (Map.elems inPlace) & join

instance Invariant (Board 'Core) where
  violation Board {playerTop, playerBottom} =
    join [violation playerTop, violation playerBottom]

main :: SharedModel -> SpecWith ()
main shared =
  describe
    "Board invariant"
    $ prop "holds initially" $
      \(Pretty teams, seed) ->
        let shared' = SharedModel.withSeed shared seed
         in (initialBoard shared' teams & snd) `shouldSatisfy` noViolation
  where
    noViolation x =
      case violation x of
        Just violation -> traceShow violation False
        Nothing -> True
