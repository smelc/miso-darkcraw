{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This module tests the balance
-- |
module Balance where

import Board
import qualified Campaign
import Card (Team (..))
import qualified Card
import Data.Function ((&))
import qualified Data.Text as Text
import Debug.Trace
import GHC.Float
import qualified Match
import Model (GameModel (..))
import SharedModel (SharedModel)
import qualified SharedModel
import Test.Hspec
import TestLib (shouldAllSatisfy)
import qualified Update

main :: SharedModel -> SpecWith ()
main shared =
  describe "Balance" $ do
    xit "Teams are balanced" $ -- Tested first, because most likely to fail
    -- Current state : "Human VS Undead: too many wins (28): Human, expected at most 23.400002"
      checkBalance Human Campaign.Level0
    xit "Starting team doesn't have an advantage" $ do
      checkBalanceStart Human Human
      checkBalanceStart Undead Undead
  where
    -- The team to test, at which level. This means rewards before
    -- this level have been obtained.
    checkBalance t level =
      let shareds = take nbMatches seeds & map (SharedModel.withSeed shared)
       in let allResults = Balance.playAll shareds t level nbTurns
           in (map snd allResults) `shouldAllSatisfy` spec
    checkBalanceStart t1 t2 =
      let shareds = take nbMatches seeds & map (SharedModel.withSeed shared)
       in let results = Balance.play shareds (Teams t1 t2) Campaign.Level0 nbTurns
           in results `shouldSatisfy` spec
    seeds = [0, 31 ..]
    nbMatches = 40
    nbTurns :: Int = 8
    spec Balance.Result {..} =
      case (min <= winTop, winTop <= max) of
        (False, _) ->
          traceShow
            ( show topTeam
                ++ " VS "
                ++ show botTeam
                ++ ": not enough wins ("
                ++ show topTeam
                ++ "): "
                ++ show winTop
                ++ ", expected at least "
                ++ show min
                ++ " (draws: "
                ++ show draws
                ++ ", "
                ++ show botTeam
                ++ " wins "
                ++ show botWins
                ++ ")"
            )
            False
        (_, False) ->
          traceShow
            ( show topTeam
                ++ " VS "
                ++ show botTeam
                ++ ": too many wins ("
                ++ show topWins
                ++ "): "
                ++ show topTeam
                ++ ", expected at most "
                ++ show max
            )
            False
        _ ->
          traceShow
            (show min ++ " <= " ++ show winTop ++ " (" ++ show topTeam ++ ") <= " ++ show max)
            True
      where
        (nbWins, winTop) = (int2Float $ topWins + botWins, int2Float topWins)
        (min, max) = (nbWins * 0.4, nbWins * 0.6)

-- | The result of executing 'play': the number of wins of each team and
-- the number of draws
data Result = Result
  { topTeam :: Team,
    topWins :: Int,
    botTeam :: Team,
    botWins :: Int,
    draws :: Int
  }
  deriving (Show)

mkEmpty :: Teams -> Balance.Result
mkEmpty Teams {topTeam, botTeam} =
  Balance.Result {topWins = 0, botWins = 0, draws = 0, ..}

playAll ::
  -- | The models to use for a start (length of this list implies the
  -- the number of games to play)
  [SharedModel] ->
  -- | The team to test
  Team ->
  -- | The level of the team
  Campaign.Level ->
  -- | The number of turns to play
  Int ->
  -- | Results against the given team
  [(Team, Balance.Result)]
playAll shareds team level nbTurns =
  [ (opponent, play shareds (Teams opponent team) level nbTurns)
    | opponent <- Card.allTeams
  ]

play ::
  -- | The models to use for a start (length of this list implies the
  -- the number of games to play)
  [SharedModel] ->
  Teams ->
  -- | The level being played
  Campaign.Level ->
  -- | The number of turns to play
  Int ->
  Balance.Result
play shareds teams _level nbTurns =
  go shareds
    & map Match.matchResult
    & count (mkEmpty teams)
  where
    go (shared : rest) =
      let result = Match.play (Update.level0GameModel shared teams) nbTurns
       in traceShow (logString result) result : go rest
    go [] = []
    count acc [] = acc
    count b@Balance.Result {draws} (Match.Draw : tail) = count b {draws = draws + 1} tail
    count tuple (Match.Error {} : tail) = count tuple tail -- not this test's business to fail on Error
    count b@Balance.Result {topWins} (Match.Win PlayerTop : tail) = count b {topWins = topWins + 1} tail
    count b@Balance.Result {botWins} (Match.Win PlayerBot : tail) = count b {botWins = botWins + 1} tail
    logString Match.Result {Match.models, Match.matchResult} =
      case matchResult of
        Match.Draw -> "Draw " ++ show (team PlayerTop) ++ " VS " ++ show (team PlayerBot)
        Match.Error err -> "[ERR] " ++ Text.unpack err
        Match.Win pSpot -> winLabel pSpot PlayerTop ++ " VS " ++ winLabel pSpot PlayerBot
      where
        lastBoard = last models & board
        team pSpot = Board.toPart lastBoard pSpot & Board.team
        winLabel winner pSpot =
          (if winner == pSpot then "Win " else "Lost")
            ++ " "
            ++ show (team pSpot)
            ++ "("
            ++ show (Board.toScore lastBoard pSpot)
            ++ ")"
