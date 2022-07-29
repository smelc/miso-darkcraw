{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Invariants where

import qualified Board
import Card
import qualified Constants
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Maybe
import Debug.Trace (traceShow)
import qualified Game
import qualified Match
import qualified Model hiding (Deck (..))
import Pretty
import qualified Shared
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Turn

class Invariant a where
  -- | Violations of the invariant if any, otherwise []
  violation :: a -> [String]

-- Could this be made more general?
instance Invariant a => Invariant [a] where
  violation [] = []
  violation (hd : rest) = violation hd ++ violation rest

instance Invariant (Creature 'Core) where
  violation c@Creature {items} =
    [ "Item " ++ show item ++ " shouldn't be on " ++ show c
      | item <- items,
        not (Game.meetsRequirement item c)
    ]

instance Invariant (Board.PlayerPart 'Core) where
  violation Board.PlayerPart {inPlace} =
    concat $ map violation (Map.elems inPlace)

instance Invariant (Board.T 'Core) where
  violation Board.T {playerTop, playerBottom} =
    violation playerTop ++ violation playerBottom

instance Invariant Turn.T where
  violation turn =
    ["Turn must be >= 1 but found " ++ show i | i < 1]
    where
      i = Turn.toNat turn

instance Invariant a => Invariant (Maybe a) where
  violation Nothing = []
  violation (Just a) = violation a

instance Invariant Model.Game where
  violation Model.Game {player, turn, uiAvail}
    | uiAvail && (Turn.toPlayerSpot turn /= Model.pSpot player) =
        ["It's the AI turn (" ++ show (Turn.toPlayerSpot turn) ++ ") yet the UI is available"]
  -- All GameModel invariants have been checked, delegate to smaller pieces:
  violation Model.Game {board, turn} = violation board ++ violation turn

main :: Shared.Model -> SpecWith ()
main shared = do
  describe "Board invariant" $ do
    prop "holds initially" $
      \(Pretty teams, seed) ->
        let shared' = Shared.withSeed shared seed
            Model.Game {board} = mkGame shared' teams
         in board `shouldSatisfy` isValid'
    prop "is preserved by playing matches" $
      \(Pretty teams, seed) ->
        let shared' = Shared.withSeed shared seed
         in Match.play (mkGame shared' teams) 32
              `shouldSatisfy` isValidResult
  describe "GameModel invariant" $ do
    prop "holds initially" $
      \teams ->
        mkGame shared teams `shouldSatisfy` isValid'
    prop "is preserved by playing matches" $
      \(Pretty teams, seed) ->
        let shared' = Shared.withSeed shared seed
         in Match.play (mkGame shared' teams) 32
              `shouldSatisfy` (\Match.Result {models} -> all isValid' models)
  where
    mkGame shared (t1, t2) =
      Match.mkTestGame
        shared
        difficulty
        (Board.Teams (t1, Shared.getInitialDeck shared t1) (t2, Shared.getInitialDeck shared t2))
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
