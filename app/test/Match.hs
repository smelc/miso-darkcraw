{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- This module simulates playing an entire game
-- |
-- play is exported for debugging with ghci
module Match (main, play) where

import qualified AI (aiPlay)
import Board
import Card
import Data.Function ((&))
import Data.List
import Data.Maybe
import Data.Text (Text)
import Debug.Trace (traceShow)
import qualified Game
import Generators ()
import Model
import SharedModel
import Test.Hspec
import Test.Hspec.QuickCheck
import Turn
import Update

main :: SharedModel -> SpecWith ()
main shared =
  describe "Playing a match doesn't return Error" $
    prop "forall t1, t2 :: Team, play shared t1 t2 8 isn't Error" $
      \(t1, t2) -> not $ isError . matchResult $ play shared t1 t2 8
  where
    isError (Error msg) = traceShow msg True
    isError Draw = False
    isError (Win _) = False

data MatchResult = Draw | Error Text | Win PlayerSpot
  deriving (Show)

data Result = Result
  { boards :: [Board 'Core],
    matchResult :: MatchResult
  }
  deriving (Show)

play :: SharedModel -> Team -> Team -> Int -> Result
play shared opponent player nbTurns =
  go (initialGameModel shared opponent player) []
  where
    go m@GameModel {turn} boards
      | turnToInt turn > nbTurns =
        Result (reverse boards) $ toMatchResult m
    go m boards =
      case playOneTurn m of
        Left msg -> Result boards $ Error msg
        Right m'@GameModel {board} -> go m' $ board : boards

toMatchResult :: GameModel -> MatchResult
toMatchResult GameModel {board} =
  if
      | scoreTop == scoreBot -> Draw
      | scoreTop > scoreBot -> Win PlayerTop
      | otherwise -> Win PlayerBottom
  where
    score pSpot = boardToPart board pSpot & Board.score
    (scoreTop, scoreBot) = (score PlayerTop, score PlayerBottom)

playOneTurn :: GameModel -> Either Text GameModel
playOneTurn m = playPlayerTurn m >>= playPlayerTurn

playPlayerTurn :: GameModel -> Either Text GameModel
playPlayerTurn m@GameModel {board, gameShared = shared, turn} =
  case AI.aiPlay shared board turn of
    [] -> go m [Update.GameEndTurnPressed]
    event : _ -> do
      -- Taking only the first event avoids the need for _correctHandIndices
      -- at the "cost" of doing recursion here:
      m' <- go m $ eventToGameActions board event
      playPlayerTurn m'
  where
    go model [] = getErr model
    go model@GameModel {interaction} (action : actions) =
      let (model', seq) = Update.updateGameModel model action interaction
       in do
            getErr model'
            go model' (map snd seq ++ actions)
    getErr m@GameModel {interaction} =
      case interaction of
        GameShowErrorInteraction err -> Left err
        _ -> Right m

eventToGameActions ::
  Board 'Core ->
  Game.Event ->
  [Update.GameAction]
eventToGameActions board event =
  case event of
    Game.Attack {} -> [Update.GamePlay event]
    Game.NoPlayEvent -> []
    Game.Place target handIndex ->
      [ Update.GameDragStart handIndex,
        Update.GameDragEnter target,
        Update.GameDrop,
        Update.GameDragEnd
      ]
    Game.Place' target _ ->
      [ Update.GameDragStart $ Game.placePrimeToHandIndex board event & fromJust,
        Update.GameDragEnter target,
        Update.GameDrop,
        Update.GameDragEnd
      ]

-- After translating the AI's events with eventToGameActions, we need
-- to handle that the events' translation introduce HandIndex values
-- which must be corrected to account for events having been played before.
_correctHandIndices _ [] = []
_correctHandIndices played (Update.GameDragStart hi@(HandIndex index) : rest) =
  Update.GameDragStart (shift played hi) : _correctHandIndices ((index : played) & sort & reverse) rest
  where
    shift [] hi = hi
    shift (played' : played_rest) hi@(HandIndex i) =
      if played' < i
        then shift played_rest (HandIndex $ i -1)
        else shift played_rest hi
_correctHandIndices played (a : rest) =
  a : _correctHandIndices played rest
