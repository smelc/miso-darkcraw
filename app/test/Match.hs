{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- This module simulates playing an entire game
-- |
module Match (main, MatchResult (..), play, Result (..), testStupidity) where

import qualified AI (Difficulty (..))
import Board
import Card
import Control.Monad.Except
import Damage (Damage (..))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace (trace, traceShow)
import Generators ()
import Model
import qualified Move
import Nat
import SharedModel
import Spots
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck ((==>))
import Turn
import Update

main :: SharedModel -> SpecWith ()
main shared = do
  describe "Playing a match doesn't return Error" $
    it "forall t1, t2 :: Team, play shared t1 t2 8 isn't Error" $
      all f teamProduct
  testStupidity shared
  where
    f (t1, t2) =
      map
        (\seed -> play (model seed) 8)
        [1 .. 48]
        & not . any (isError . matchResult . traceResult)
      where
        model seed = level0GameModel AI.Easy (SharedModel.withSeed shared seed) $ Teams t1 t2
    isError (Error msg) = traceShow msg True
    isError Draw = False
    isError (Win _) = False
    teamProduct = [(t1, t2) | t1 <- allTeams, t2 <- allTeams]
    traceResult r =
      case (matchResult r, last $ (map board . models) r) of
        (Error msg, _) -> traceShow ("[ERR] " ++ Text.unpack msg) r
        (Draw, board) -> traceShow ("Draw " ++ showScores board) r
        (Win _, board) -> traceShow ("Win " ++ showScores board) r
    last l = reverse l & listToMaybe
    showScores board =
      showScore board PlayerTop
        ++ " "
        ++ sign board
        ++ " "
        ++ showScore board PlayerBot
      where
        sign (Just (board :: Board 'Core)) =
          case compare (Board.toScore PlayerTop board) (Board.toScore PlayerBot board) of
            LT -> "<"
            EQ -> "="
            GT -> ">"
        sign Nothing = "?"
        showScore Nothing _ = "?"
        showScore (Just (board :: Board 'Core)) pSpot =
          show (Board.toPart board pSpot & Board.team)
            ++ " "
            ++ show (Board.toScore pSpot board)

testStupidity :: SharedModel -> SpecWith ()
testStupidity shared =
  describe "Stupidity is handled correctly" $
    prop "the score is correctly predicted" $
      \(cSpot, topTeam, seed) ->
        not (inTheBack cSpot)
          ==> let teams = Teams topTeam Human
                  board = initialBoard shared teams cSpot
                  model = Update.unsafeInitialGameModel AI.Easy (mkShared seed) (mkTeamData teams) board
               in play model 8 `shouldSatisfy` isValid
  where
    initialBoard s teams cSpot = Board.small s teams ogreID [] startingPlayerSpot cSpot
    ogreID = CreatureID Card.Ogre Human
    ogreSpot = PlayerBot
    Damage {base = ogreAttack} = SharedModel.idToCreature shared ogreID [] & fromJust & attack
    mkShared seed = SharedModel.withSeed shared seed
    mkTeamData teams = teams <&> (\t -> (t, SharedModel.getInitialDeck shared t))
    isValid Result {models} = all isValidModel models
    isValidModel Model.Game {board, turn} =
      expectedScore == score
        || trace (show expectedScore ++ "<>" ++ show score ++ " at turn " ++ show turn) False
      where
        score = Board.toScore ogreSpot board
        turni = Turn.toNat turn
        pSpot = Turn.toPlayerSpot turn
        stupidFreq = 4
        nbAttacks played = (turni - (if played then 0 else 1)) - ((turni - 1) `div` stupidFreq)
        expectedScore = nbAttacks (pSpot /= ogreSpot) * ogreAttack

data MatchResult = Draw | Error Text | Win Spots.Player
  deriving (Show)

data Result = Result
  { models :: [Model.Game],
    matchResult :: MatchResult
  }
  deriving (Show)

play ::
  Model.Game ->
  -- The number of turns
  Nat ->
  Result
play model nbTurns =
  go model []
  where
    go m@Model.Game {turn} models
      | Turn.toNat turn > nbTurns =
        Result (reverse models) (toMatchResult m)
    go m models =
      case playOneTurn m of
        Left msg -> Result (reverse models) (Error msg)
        Right m' -> go m' $ m : models

toMatchResult :: Model.Game -> MatchResult
toMatchResult Model.Game {board}
  | scoreTop == scoreBot = Draw
  | scoreTop > scoreBot = Win PlayerTop
  | otherwise = Win PlayerBot
  where
    score pSpot = Board.toPart board pSpot & Board.score
    (scoreTop, scoreBot) = (score PlayerTop, score PlayerBot)

-- | Play one turn. XXX: return the intermediate 'GameModel's, because they
-- make sense for being checked with properties from 'Invariants'.
playOneTurn :: MonadError Text m => a ~ Model.Game => a -> m a
playOneTurn m = Move.onContainedE playOneTurnK m

playOneTurnK :: MonadError Text m => a ~ Move.Kernel => a -> m a
playOneTurnK m = do
  m <- playPart startingPlayerSpot m
  playPart (Spots.other startingPlayerSpot) m
  where
    playPart pSpot m = do
      (m, nextSched) <- Move.startTurn Move.AI pSpot m
      Move.runAllMaybe nextSched m
