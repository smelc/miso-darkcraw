{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Invariants where

import Board
import qualified Campaign
import Card
import qualified Constants
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe
import Debug.Trace (traceShow)
import qualified Match
import qualified Model hiding (Deck (..))
import Pretty
import qualified Shared
import qualified Spots
import Test.Hspec
import Test.Hspec.QuickCheck
import Turn
import qualified Update

class Invariant a where
  -- | Violations of the invariant if any, otherwise []
  violation :: a -> [String]

-- Could this be made more general?
instance Invariant a => Invariant [a] where
  violation [] = []
  violation (hd : rest) = violation hd ++ violation rest

instance Invariant (PlayerPart 'Core) where
  violation PlayerPart {..} =
    ["Score should be >= 0 but found " ++ show score | score < 0]

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

instance Invariant Model.Game where
  violation Model.Game {playingPlayer, turn, uiAvail}
    | uiAvail && (Turn.toPlayerSpot turn /= playingPlayer) =
        ["It's the AI turn (" ++ show (Turn.toPlayerSpot turn) ++ ") yet the UI is available"]
  -- All GameModel invariants have been checked, delegate to smaller pieces:
  violation Model.Game {board, turn} = violation board ++ violation turn

main :: Shared.Model -> SpecWith ()
main shared = do
  describe "Board invariant" $ do
    prop "holds initially" $
      \(Pretty teams, seed) ->
        let shared' = Shared.withSeed shared seed
            Model.Game {board} = Update.level0GameModel difficulty shared' (mkJourney teams) teams
         in board `shouldSatisfy` isValid'
    prop "is preserved by playing matches" $
      \(Pretty teams, seed) ->
        let shared' = Shared.withSeed shared seed
         in Match.play (Update.level0GameModel difficulty shared' (mkJourney teams) teams) 32
              `shouldSatisfy` isValidResult
  describe "GameModel invariant" $ do
    prop "holds initially" $
      \(difficulty, teams) ->
        Update.level0GameModel difficulty shared (mkJourney teams) teams `shouldSatisfy` isValid'
    prop "is preserved by playing matches" $
      \(Pretty teams, seed) ->
        let shared' = Shared.withSeed shared seed
         in Match.play (Update.level0GameModel difficulty shared' (mkJourney teams) teams) 32
              `shouldSatisfy` (\Match.Result {models} -> all isValid' models)
  where
    mkJourney teams =
      Campaign.unsafeJourney
        Campaign.Level0
        (Board.toData (Spots.other Spots.startingPlayerSpot) teams)
    difficulty = Constants.Easy
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
