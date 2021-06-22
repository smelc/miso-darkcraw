{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This module tests the balance
-- |
module Balance where

import qualified AI
import Board
import qualified Campaign
import Card
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List
import Data.Maybe
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
    it "The teams' balance is as expected" $ do
      -- Tested first, because most likely to fail
      checkBalance Human Campaign.Level0
      checkBalance Human Campaign.Level1
    xit "Starting team doesn't have an advantage" $ do
      checkBalanceStart Undead Undead
      checkBalanceStart Human Human
  where
    -- The team to test, at which level. This means rewards before
    -- this level have been obtained.
    checkBalance t level =
      let shareds = take nbMatches seeds & map (SharedModel.withSeed shared)
       in let allResults = Balance.playAll shareds t level nbTurns
           in (map snd allResults) `shouldAllSatisfy` actualSpec
    checkBalanceStart t1 t2 =
      let shareds = take nbEndoMatches seeds & map (SharedModel.withSeed shared)
       in let results = Balance.play shareds Campaign.Level0 teams nbTurns
           in results `shouldSatisfy` actualSpec
      where
        teams = Teams t1 t2 <&> (\t -> (t, Card.teamDeck uiCards t))
        uiCards = SharedModel.getCards shared
    seeds = [0, 31 ..]
    (nbMatches, nbEndoMatches) = (40, nbMatches `div` 2)
    nbTurns :: Int = 8
    -- actualSpec is a cheap way to track changes to the balance
    actualSpec r@Balance.Result {..}
      | topTeam == Human
          && botTeam == Undead
          && topWins == 22
          && botWins == 17
          && level == Campaign.Level0 =
        traceShow (showBalanced r) True
    actualSpec r@Balance.Result {..}
      | topTeam == Human
          && botTeam == Undead
          && topWins == 22
          && botWins == 17
          && level == Campaign.Level1 =
        traceShow (showBalanced r) True
    actualSpec r =
      traceShow ("Unexpected spec: " ++ show r) False
    _desiredSpec balanceRes@Balance.Result {..} =
      case (min <= winTop, winTop <= max) of
        (False, _) -> traceShow (showNotEnoughTopWins balanceRes) False
        (_, False) -> traceShow (showTooManyTopWins balanceRes) False
        _ -> traceShow (showBalanced balanceRes) True
      where
        nbGames = int2Float $ topWins + botWins + draws
        winTop = int2Float topWins
        (min, max) = (nbGames * 0.4, nbGames * 0.6)

-- | The result of executing 'play': the number of wins of each team and
-- the number of draws
data Result = Result
  { level :: Campaign.Level,
    topTeam :: Team,
    topWins :: Int,
    botTeam :: Team,
    botWins :: Int,
    draws :: Int
  }
  deriving (Show)

mkEmpty :: Campaign.Level -> Teams Team -> Balance.Result
mkEmpty level Teams {topTeam, botTeam} =
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
  [ (opponent, play shareds level teams nbTurns)
    | opponent <- otherTeams,
      teamDeck <- decks team,
      opponentDeck <- decks opponent,
      let teams = Teams (team, teamDeck) (opponent, opponentDeck)
  ]
  where
    otherTeams = [t | t <- Card.allTeams, t /= team]
    ids t =
      Campaign.decks
        (SharedModel.getInitialDeck shared t & map Card.cardToIdentifier)
        level
        t
    decks t =
      [ map (SharedModel.identToCard shared) cards
          & catMaybes
          & map Card.unliftCard
        | cards <- ids t
      ]
    shared = head shareds

play ::
  -- | The models to use for a start (length of this list implies the
  -- the number of games to play)
  [SharedModel] ->
  -- | The level being used
  Campaign.Level ->
  -- | The decks to use
  Teams (Team, [Card 'Core]) ->
  -- | The number of turns to play
  Int ->
  Balance.Result
play shareds level teams nbTurns =
  go shareds
    & map Match.matchResult
    & count (mkEmpty level $ fmap fst teams)
  where
    go (shared : rest) =
      let result = Match.play (Update.levelNGameModel AI.Easy shared teams) nbTurns
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
            ++ "(score: "
            ++ show (Board.toScore lastBoard pSpot)
            ++ ", "
            ++ show (length cards)
            ++ " cards: "
            ++ concat (intersperse " " (map freqIdToStr cardsFreq))
            ++ ")"
          where
            cards = toData pSpot teams & snd
            cardsFreq =
              map (cardToIdentifier) cards
                & groupBy (==)
                & map (\repeats -> (head repeats, length repeats))
            idToStr (IDC (CreatureID {creatureKind}) []) = take 2 $ show creatureKind
            idToStr (IDC cid _) = idToStr (IDC cid []) ++ "..."
            idToStr (IDI i) = show i
            idToStr (IDN n) = show n
            freqIdToStr (id, 1) = idToStr id
            freqIdToStr (id, i) = show i ++ idToStr id

showNotEnoughTopWins :: Result -> String
showNotEnoughTopWins Balance.Result {..} =
  show topTeam
    ++ " VS "
    ++ show botTeam
    ++ ": not enough wins ("
    ++ show topTeam
    ++ "/"
    ++ show nbGames
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
    ++ ", level "
    ++ show level
    ++ ")"
  where
    (_, winTop) = (int2Float $ topWins + botWins, int2Float topWins)
    nbGames = int2Float $ botWins + draws + topWins
    (min, _max) = minMax nbGames

showTooManyTopWins :: Result -> String
showTooManyTopWins Balance.Result {..} =
  show topTeam
    ++ " VS "
    ++ show botTeam
    ++ ": too many wins ("
    ++ show topWins
    ++ "/"
    ++ show nbGames
    ++ "): "
    ++ show topTeam
    ++ ", expected at most "
    ++ show max
    ++ " (level: "
    ++ show level
    ++ ")"
  where
    nbGames = int2Float $ botWins + draws + topWins
    (_min, max) = minMax nbGames

showBalanced :: Result -> String
showBalanced Balance.Result {..} =
  (show min ++ " <= " ++ show winTop ++ " (" ++ show topTeam ++ ") <= " ++ show max ++ ", " ++ show level)
  where
    nbGames = int2Float $ topWins + botWins + draws
    winTop = int2Float topWins
    (min, max) = minMax nbGames

minMax :: Float -> (Float, Float)
minMax nbGames = (nbGames * 0.4, nbGames * 0.6)
