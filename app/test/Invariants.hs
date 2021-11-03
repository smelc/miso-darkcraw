{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Invariants where

import qualified AI
import Board
import Card
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Maybe
import Debug.Trace (traceShow)
import qualified Match
import Model (GameModel (..))
import Pretty
import SharedModel
import Test.Hspec
import Test.Hspec.QuickCheck
import Turn
import qualified Update

class Invariant a where
  -- | Violations of the invariant if any, otherwise []
  violation :: a -> [String]

instance Invariant (Creature 'Core) where
  violation Creature {..} =
    ["Creature's attack should be >= 0 but found " ++ show attack | attack < 0]
      ++ ["Creature's hp should be >= 0 but found " ++ show hp | hp < 0]

-- Could this be made more general?
instance Invariant a => Invariant [a] where
  violation [] = []
  violation (hd : rest) = violation hd ++ violation rest

instance Invariant (PlayerPart 'Core) where
  violation PlayerPart {..} =
    ["Score should be >= 0 but found " ++ show score | score < 0]
      ++ (map violation (Map.elems inPlace) & concat)

instance Invariant (Board 'Core) where
  violation Board {playerTop, playerBottom} =
    violation playerTop ++ violation playerBottom

instance Invariant Turn where
  violation turn =
    ["Turn must be >= 1 but found " ++ show i | i < 1]
    where
      i = Turn.toNat turn

instance Invariant a => Invariant (Maybe a) where
  violation Nothing = []
  violation (Just a) = violation a

instance Invariant GameModel where
  violation GameModel {playingPlayer, turn, uiAvail}
    | uiAvail && (Turn.toPlayerSpot turn /= playingPlayer) =
      ["It's the AI turn (" ++ show (Turn.toPlayerSpot turn) ++ ") yet the UI is available"]
  -- All GameModel invariants have been checked, delegate to smaller pieces:
  violation GameModel {board, turn} = violation board ++ violation turn

main :: SharedModel -> SpecWith ()
main shared = do
  describe "Board invariant" $ do
    prop "holds initially" $
      \(Pretty teams, seed) ->
        let shared' = SharedModel.withSeed shared seed
         in let GameModel {board} = Update.level0GameModel difficulty shared' teams
             in board `shouldSatisfy` isValid'
    prop "is preserved by playing matches" $
      \(Pretty team1, Pretty team2, seed) ->
        let shared' = SharedModel.withSeed shared seed
         in Match.play (Update.level0GameModel difficulty shared' $ Teams team1 team2) 32
              `shouldSatisfy` isValidResult
  describe "GameModel invariant" $ do
    prop "holds initially" $
      \(difficulty, teams) ->
        Update.level0GameModel difficulty shared teams `shouldSatisfy` isValid'
    prop "is preserved by playing matches" $
      \(Pretty team1, Pretty team2, seed) ->
        let shared' = SharedModel.withSeed shared seed
         in Match.play (Update.level0GameModel difficulty shared' $ Teams team1 team2) 32
              `shouldSatisfy` (\Match.Result {models} -> all isValid' models)
  where
    difficulty = AI.Easy
    isValid :: [String] -> Bool
    isValid [] = True
    isValid violations = traceShow (unlines violations) False
    isValid' :: Invariant a => a -> Bool
    isValid' x = isValid $ violation x
    isValidResult Match.Result {models} =
      isValid $ violation boards ++ violation turnResult
      where
        boards = map Model.board models
        turnResult = reverse models & listToMaybe <&> Model.turn
