{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Debug.Trace
import qualified Match
import Model (GameModel (..))
import Nat
import SharedModel (SharedModel)
import qualified SharedModel
import Spots hiding (Card)
import Test.Hspec
import qualified Update
import qualified Weight

main :: SharedModel -> SpecWith ()
main shared =
  describe "Balance" $ do
    it "Balance data is well formed" $ do
      length balances `shouldBe` Set.size (Set.fromList balances)
    it "The teams' balance is as expected" $ do
      -- Level0
      check Human Campaign.Level0 Evil
      check Human Campaign.Level0 Undead
      check Human Campaign.Level0 ZKnights
      check Evil Campaign.Level0 Undead
      -- Level1
      check Human Campaign.Level1 Evil
      check Human Campaign.Level1 Undead
      check Evil Campaign.Level1 Undead
    xit "Starting team doesn't have an advantage" $ do
      check Human Campaign.Level0 Human
      check Evil Campaign.Level0 Evil
      check Undead Campaign.Level0 Undead
  where
    check team level opponent =
      shouldBe True $
        if actual == expected
          then traceShow (prefix ++ details team actual opponent) True
          else traceShow (prefix ++ show expected ++ " is WRONG. Witnessed: " ++ show actual) False
      where
        actual = Balance.playAll (mkShareds nbMatches) team level nbTurns opponent & mconcat
        prefix = show team ++ "/" ++ show opponent ++ " matchup at " ++ show level ++ ": "
        expected = Weight.find team level opponent & fromJust & (\(i, j, k) -> Stat i j k)
    seeds = [0, 31 ..]
    mkShareds n = take n seeds & map (SharedModel.withSeed shared)
    nbMatches = 64
    nbTurns :: Nat = 8
    balances = Weight.balances & map (\(x, y, z, _, _, _) -> (x, y, z))

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

playAll ::
  -- | The models to use for a start (length of this list implies the
  -- the number of games to play)
  [SharedModel] ->
  -- | The team to test
  Team ->
  -- | The level of the team
  Campaign.Level ->
  -- | The number of turns to play
  Nat ->
  -- | The opponent team
  Team ->
  -- | Results against the given team
  [Stat]
playAll shareds team level nbTurns opponent =
  [ play shareds level teams nbTurns
    | teamDeck <- decks team,
      opponentDeck <- decks opponent,
      let teams = Teams (team, teamDeck) (opponent, opponentDeck)
  ]
  where
    ids t =
      Campaign.augment
        (SharedModel.getInitialDeck shared t & map Card.cardToIdentifier)
        level
        t
    decks t =
      [ mapMaybe (SharedModel.identToCard shared) cards
          & map Card.unlift
        | cards <- ids t
      ]
    shared = head shareds

playOne ::
  -- | The models to use for a start (length of this list implies the
  -- the number of games to play)
  [SharedModel] ->
  -- | The team to test
  Team ->
  -- | The level of the team
  Campaign.Level ->
  -- | The team to test against: the team itself and its deck
  (Team, [Card 'Core]) ->
  -- | The number of turns to play
  Nat ->
  -- | Results
  [Stat]
playOne shareds team level (opponent, opponentDeck) nbTurns =
  [ play shareds level teams nbTurns
    | -- Try permutations of opponentDeck, for variety
      (teamDeck, otherDeck) <- zip (decks team) (permutations opponentDeck),
      let teams = Teams (team, teamDeck) (opponent, otherDeck)
  ]
  where
    ids t =
      Campaign.augment
        (SharedModel.getInitialDeck shared t & map Card.cardToIdentifier)
        level
        t
    decks t =
      [ mapMaybe (SharedModel.identToCard shared) cards & map Card.unlift
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
  Nat ->
  Stat
play shareds level teams nbTurns =
  go shareds
    & map Match.matchResult
    & count mempty
  where
    go (shared : rest) =
      let result = Match.play (Update.levelNGameModel AI.Easy shared level teams) nbTurns
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
        lastBoard = last models & board
        team pSpot = Board.toPart lastBoard pSpot & Board.team
        winLabel winner pSpot =
          (if winner == pSpot then "Win " else "Lost")
            ++ " "
            ++ show (team pSpot)
            ++ "(score: "
            ++ show (Board.toScore pSpot lastBoard)
            ++ ", "
            ++ show (length cards)
            ++ " cards: "
            ++ unwords (map freqIdToStr cardsFreq)
            ++ ")"
          where
            cards = toData pSpot teams & snd
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
