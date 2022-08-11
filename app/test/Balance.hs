{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- This module tests the balance
-- |
module Balance where

import qualified Board
import Card
import qualified Constants
import Data.Function ((&))
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as Text
import Debug.Trace
import qualified Match
import qualified Model (Game (..))
import Nat
import qualified Network
import qualified Shared
import Spots hiding (Card)
import Test.Hspec
import qualified Weight

main :: Shared.Model -> SpecWith ()
main shared =
  describe "Balance" $ do
    it "The teams' balance is as expected" $ do
      checkList (start Human) [Undead, Beastmen, Evil] `shouldBe` True
      checkList (start Human) [Undead, Sylvan, ZKnights] `shouldBe` True
  where
    start team =
      ( team,
        Shared.getInitialDeck shared team,
        Network.rewards
          & Map.lookup team
          & fromJust
          & map (\(Network.Rewards _ cards) -> head cards) -- Take first reward every time
          & map (Shared.unsafeKeyToCard shared)
          & map Card.unlift
      )
    -- it "Endomatches balance is as expected" $ do
    --   check Beastmen Campaign.Level0 Beastmen
    --   check Human Campaign.Level0 Human
    --   check Evil Campaign.Level0 Evil
    --   check Sylvan Campaign.Level0 Sylvan
    --   check Undead Campaign.Level0 Undead
    checkList (team :: Team, teamDeck :: [Card 'Core], rewards :: [Card 'Core]) (opponents :: [Team]) =
      go (team, teamDeck, rewards) expected (0 :: Nat)
      where
        expected :: [(Team, Stat)] =
          Weight.find team opponents
            & ( \case
                  Nothing -> error $ "Weight.find " ++ show team ++ " " ++ show opponents ++ " returned Nothing"
                  Just x -> x
              )
            & map (\(i, j, k) -> Stat i j k)
            & zip opponents
        go (team, deck, rewards) opponents idx =
          case opponents of
            [] -> True
            ((opponent, expected) : opponents) ->
              checkOne (team, deck) opponent idx expected
                && go (team, nextDeck, nextRewards) opponents (idx + 1)
              where
                (nextDeck, nextRewards) =
                  case rewards of
                    (reward : rewards) -> (reward : deck, rewards) -- Add first reward to obtain next deck
                    [] -> (deck, [])
    checkOne (team, teamDeck) opponent idx expected =
      case actual == expected of
        True -> traceShow (prefix ++ details team actual opponent) True
        False -> traceShow (prefix ++ show expected ++ " is WRONG. Witnessed: " ++ show actual) False
      where
        actual = Balance.play (mkShareds nbMatches) teams nbTurns
        prefix = show team ++ "/" ++ show opponent ++ " matchup[" ++ show idx ++ "] "
        -- TODO build different opponent deck depending on journey progression?
        opponentDeck = Shared.getInitialDeck shared opponent
        teams = Board.Teams (team, teamDeck) (opponent, opponentDeck)
    seeds = [0, 31 ..]
    mkShareds n = take n seeds & map (Shared.withSeed shared)
    nbMatches = 64
    nbTurns :: Nat = 8

data Stat = Stat {topWins :: Nat, botWins :: Nat, draws :: Nat}
  deriving (Eq, Show)

instance Semigroup Stat where
  Stat {topWins = t1, botWins = b1, draws = d1} <> Stat {topWins = t2, botWins = b2, draws = d2} =
    Stat {topWins = t1 + t2, botWins = b1 + b2, draws = d1 + d2}

instance Monoid Stat where
  mempty = Stat {topWins = 0, botWins = 0, draws = 0}

details :: Team -> Stat -> Team -> String
details top Stat {topWins, botWins, draws} bot =
  (showLine top topWins ++ " VS ")
    ++ (showLine bot botWins)
    ++ (" [" ++ show draws ++ " draws (" ++ showP (mkPercent draws) ++ ")]")
  where
    total :: Float = fromIntegral $ topWins + botWins + draws
    mkPercent x = (100.0 / total) * (fromIntegral x)
    showP (x :: float) = (takeWhile ((/=) '.') $ show x) ++ "%"
    showLine team wins =
      show team ++ " " ++ show wins ++ " wins (" ++ showP (mkPercent wins) ++ ")"

play ::
  -- | The models to use for a start (length of this list implies the
  -- the number of games to play)
  [Shared.Model] ->
  -- | The decks to use
  Board.Teams (Team, [Card 'Core]) ->
  -- | The number of turns to play
  Nat ->
  Stat
play shareds teams nbTurns =
  go shareds
    & map Match.matchResult
    & count mempty
  where
    go (shared : rest) =
      -- I tried setting AI to Hard once and it didn't change the outcome significantly,
      -- except that it was wayyyyy slower.
      let result = Match.play (Match.mkTestGame shared Constants.Easy teams) nbTurns
       in result : go rest
    go [] = []
    count :: Stat -> [Match.MatchResult] -> Stat
    count acc [] = acc
    count s (Match.Error {} : tail) = count s tail -- not this test's business to fail on Error
    count s@Stat {draws} (Match.Draw : tail) = count s {draws = draws + 1} tail
    count s@Stat {topWins} (Match.Win PlayerTop : tail) = count s {topWins = topWins + 1} tail
    count s@Stat {botWins} (Match.Win PlayerBot : tail) = count s {botWins = botWins + 1} tail
    _logString Match.Result {Match.models, Match.matchResult} =
      case matchResult of
        Match.Draw -> "Draw " ++ show (team PlayerTop) ++ " VS " ++ show (team PlayerBot)
        Match.Error err -> "[ERR] " ++ Text.unpack err
        Match.Win pSpot -> winLabel pSpot PlayerTop ++ " VS " ++ winLabel pSpot PlayerBot
      where
        lastBoard = last models & Model.board
        team pSpot = Board.toPart lastBoard pSpot & Board.team
        winLabel winner pSpot =
          (if winner == pSpot then "Win " else "Lost")
            ++ " "
            ++ show (team pSpot)
            ++ "(score: "
            ++ show (Board.getpk @'Board.Score pSpot lastBoard)
            ++ ", "
            ++ show (length cards)
            ++ " cards: "
            ++ unwords (map freqIdToStr cardsFreq)
            ++ ")"
          where
            cards = Board.toData pSpot teams & snd
            cardsFreq =
              map cardToIdentifier cards
                & group
                & map (\repeats -> (head repeats, length repeats))
            idToStr (IDC (CreatureID {creatureKind}) []) = take 2 $ show creatureKind
            idToStr (IDC cid _) = idToStr (IDC cid []) ++ "..."
            idToStr (IDI i) = show i
            idToStr (IDN n) = show n
            freqIdToStr (id, 1) = idToStr id
            freqIdToStr (id, i) = show i ++ idToStr id
