{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This module simulates playing an entire game
-- |
module Match (main, MatchResult (..), play, Result (..)) where

import qualified AI (play)
import Board
import Card
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace (trace, traceShow)
import qualified Game
import Generators ()
import Model
import SharedModel
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (withMaxSuccess, (==>))
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
        [1, 2]
        & not . any (isError . matchResult . traceResult)
      where
        model seed = initialGameModel (SharedModel.withSeed shared seed) t1 t2
    isError (Error msg) = traceShow msg True
    isError Draw = False
    isError (Win _) = False
    teamProduct = [(t1, t2) | t1 <- allTeams, t2 <- allTeams]
    traceResult r =
      case (matchResult r, last $ (map board . models) r) of
        (Error msg, _) -> traceShow ("ERROR " ++ Text.unpack msg) r
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
          case compare (scoreTop board) (scoreBot board) of
            LT -> "<"
            EQ -> "="
            GT -> ">"
        sign Nothing = "?"
        scoreTop board = boardToScore board PlayerTop
        scoreBot board = boardToScore board PlayerBot
        showScore Nothing _ = "?"
        showScore (Just (board :: Board 'Core)) pSpot =
          show (boardToPart board pSpot & Board.team)
            ++ " "
            ++ show (boardToScore board pSpot)

testStupidity shared =
  describe "Stupidity is handled correctly" $
    prop "the score is correctly predicted" $
      \(cSpot, topTeam, seed) ->
        not (inTheBack cSpot)
          ==> let (teams, board) = (Teams topTeam Human, initialBoard shared teams cSpot)
               in let model = Update.unsafeInitialGameModel (mkShared seed) board
                   in play model 8 `shouldSatisfy` isValid
  where
    initialBoard s teams cSpot = smallBoard s teams ogreID [] startingPlayerSpot cSpot
    ogreID = CreatureID Card.Ogre Human
    ogreSpot = PlayerBot
    ogreAttack = SharedModel.idToCreature shared ogreID [] & fromJust & attack
    mkShared seed = SharedModel.withSeed shared seed
    isValid Result {models} = all isValidModel models
    isValidModel GameModel {board, turn} =
      expectedScore == score
        || trace (show expectedScore ++ "<>" ++ show score ++ " at turn " ++ show turn) False
      where
        score = boardToScore board ogreSpot
        turni = turnToInt turn
        pSpot = turnToPlayerSpot turn
        stupidFreq = 4
        nbAttacks played = (turni - (if played then 0 else 1)) - ((turni - 1) `div` stupidFreq)
        expectedScore = nbAttacks (pSpot /= ogreSpot) * ogreAttack

data MatchResult = Draw | Error Text | Win PlayerSpot
  deriving (Show)

data Result = Result
  { models :: [GameModel],
    matchResult :: MatchResult
  }
  deriving (Show)

play ::
  GameModel ->
  -- The number of turns
  Int ->
  Result
play model nbTurns =
  go model []
  where
    go m@GameModel {turn} models
      | turnToInt turn > nbTurns =
        Result (reverse models) (toMatchResult m)
    go m@GameModel {turn} models =
      case playOneTurn m of
        Left msg -> Result (reverse models) (Error msg)
        Right m' -> go m' $ m : models

toMatchResult :: GameModel -> MatchResult
toMatchResult GameModel {board} =
  if
      | scoreTop == scoreBot -> Draw
      | scoreTop > scoreBot -> Win PlayerTop
      | otherwise -> Win PlayerBot
  where
    score pSpot = boardToPart board pSpot & Board.score
    (scoreTop, scoreBot) = (score PlayerTop, score PlayerBot)

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
      -- Taking only the first event avoids the need for correcting
      -- hand indices at the "cost" of doing recursion here:
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
        ShowErrorInteraction err -> Left err
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
      [ Update.DragStart handIndex,
        Update.DragEnter target,
        Update.Drop,
        Update.DragEnd
      ]
        & map GameDnD
    Game.Place' target _ ->
      [ Update.DragStart $ Game.placePrimeToHandIndex board event & fromJust,
        Update.DragEnter target,
        Update.Drop,
        Update.DragEnd
      ]
        & map GameDnD
