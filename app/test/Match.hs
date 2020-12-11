{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This module simulates playing an entire game
-- |
-- play is exported for debugging with ghci
module Match (main, play) where

import qualified AI (play)
import Board
import Card
import Data.Function ((&))
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace (traceShow, traceShowId)
import qualified Game
import Generators ()
import Model
import SharedModel
import Test.Hspec
import Turn
import Update

main :: SharedModel -> SpecWith ()
main shared =
  describe "Playing a match doesn't return Error" $
    it "forall t1, t2 :: Team, play shared t1 t2 8 isn't Error" $
      all f teamProduct
  where
    f (t1, t2) =
      map
        (\seed -> play (SharedModel.withSeed shared seed) t1 t2 8)
        [1, 2]
        & not . any (isError . matchResult . traceResult)
    isError (Error msg) = traceShow msg True
    isError Draw = False
    isError (Win _) = False
    teamProduct = [(t1, t2) | t1 <- allTeams, t2 <- allTeams]
    traceResult r =
      case (matchResult r, last $ boards r) of
        (Error msg, _) -> traceShow ("ERROR " ++ Text.unpack msg) r
        (Draw, board) -> traceShow ("Draw " ++ showScores board) r
        (Win _, board) -> traceShow ("Win " ++ showScores board) r
    last l = reverse l & listToMaybe
    showScores board =
      showScore board PlayerTop
        ++ " "
        ++ sign board
        ++ " "
        ++ showScore board PlayerBottom
      where
        sign (Just (board :: Board 'Core)) =
          case compare (scoreTop board) (scoreBot board) of
            LT -> "<"
            EQ -> "="
            GT -> ">"
        sign Nothing = "?"
        scoreTop board = boardToScore board PlayerTop
        scoreBot board = boardToScore board PlayerBottom
        -- TODO @smelc Track teams in Board
        showScore Nothing _ = "?"
        showScore (Just (board :: Board 'Core)) pSpot = show $ boardToScore board pSpot

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
playOneTurn m@GameModel {board, gameShared = shared, playingPlayer, turn} =
  -- We need to play for the player
  case (playingPlayer == pSpot, AI.play shared board turn) of
    (False, _) -> Left $ Text.pack $ "It should be the player turn (" ++ show playingPlayer ++ "), but found: " ++ show pSpot
    (_, []) ->
      -- The main loop will take care of playing the opponent when honoring
      -- this event:
      go m [Update.GameEndTurnPressed]
    (_, event : _) -> do
      -- Taking only the first event avoids the need for _correctHandIndices
      -- at the "cost" of doing recursion here:
      m' <- go m $ eventToGameActions board event
      playOneTurn m'
  where
    pSpot = turnToPlayerSpot turn
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
