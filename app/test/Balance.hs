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
import Data.Functor ((<&>))
import Data.List
import Data.Maybe
import qualified Data.Text as Text
import Debug.Trace
import GHC.Float
import qualified Match
import Model (GameModel (..))
import Nat
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
    (nbMatches, nbEndoMatches) = (64, nbMatches)
    nbTurns :: Nat = 8
    -- actualSpec is a cheap way to track changes to the balance
    actualSpec r@Balance.Result {..} =
      case find (satisfies r) specs of
        Nothing -> traceShow ("Unexpected spec: " ++ show r) False
        Just spec -> traceShow (show spec) True
    specs :: [Balance.Spec] = map Eq eqSpecs ++ map Leq (leqSpecs ++ zknights)
    eqSpecs =
      -- Level0
      map
        (uncurry FullEqSpec)
        [ ( SpecShared {topTeam = Human, botTeam = Undead, level = Campaign.Level0},
            EqSpec {topWins = 40, botWins = 24}
          ),
          ( SpecShared {topTeam = Human, botTeam = Evil, level = Campaign.Level0},
            EqSpec {topWins = 55, botWins = 9}
          )
        ]
    leqSpecs =
      -- Level1
      map
        (uncurry FullLeqSpec)
        [ ( SpecShared {topTeam = Human, botTeam = Undead, level = Campaign.Level1},
            LeqSpec {topMaxWins = 37, botMinWins = 26}
          ),
          ( SpecShared {topTeam = Human, botTeam = Evil, level = Campaign.Level1},
            LeqSpec {topMaxWins = 49, botMinWins = 15}
          )
        ]
    zknights =
      -- dumb specs for Level0 Level1. FIXME @smelc Provide true specs.
      map
        (uncurry FullLeqSpec)
        [ ( SpecShared {topTeam = Human, botTeam = ZKnights, level = Campaign.Level0},
            LeqSpec {topMaxWins = nbMatches, botMinWins = 0}
          ),
          ( SpecShared {topTeam = Human, botTeam = ZKnights, level = Campaign.Level1},
            LeqSpec {topMaxWins = nbMatches, botMinWins = 0}
          )
        ]
    _desiredSpec balanceRes@Balance.Result {..} =
      case (min <= winTop, winTop <= max) of
        (False, _) -> traceShow (showNotEnoughTopWins balanceRes) False
        (_, False) -> traceShow (showTooManyTopWins balanceRes) False
        _ -> traceShow (showBalanced balanceRes) True
      where
        nbGames = int2Float $ topWins + botWins + draws
        winTop = int2Float topWins
        (min, max) = (nbGames * 0.4, nbGames * 0.6)

-- | An expectation of the Balance
data Spec
  = Leq FullLeqSpec
  | Eq FullEqSpec

instance Show Balance.Spec where
  show (Leq spec) = show spec
  show (Eq spec) = show spec

-- | A pair of a 'SpecShared' and a 'EqSpec'
data FullEqSpec = FullEqSpec SpecShared EqSpec

instance Show FullEqSpec where
  show (FullEqSpec (SpecShared {topTeam, botTeam}) (EqSpec {topWins, botWins})) =
    show topTeam ++ " (top): " ++ show topWins ++ " wins, " ++ show botTeam
      ++ " (bot):"
      ++ show botWins
      ++ " wins"

-- | A pair of a 'SpecShared' and a 'LeqSpec'
data FullLeqSpec = FullLeqSpec SpecShared LeqSpec

instance Show FullLeqSpec where
  show (FullLeqSpec (SpecShared {topTeam, botTeam}) (LeqSpec {topMaxWins, botMinWins})) =
    show botMinWins ++ " <= " ++ show botTeam ++ " <= _, "
      ++ "_ <= "
      ++ show topTeam
      ++ " <= "
      ++ show topMaxWins

class Satisfies a where
  satisfies :: Balance.Result -> a -> Bool

instance Satisfies Balance.Spec where
  satisfies r (Leq (FullLeqSpec shared sub)) = satisfies r shared && satisfies r sub
  satisfies r (Eq (FullEqSpec shared sub)) = satisfies r shared && satisfies r sub

instance Satisfies SpecShared where
  satisfies
    Balance.Result {topTeam = actualTopTeam, botTeam = actualBotTeam, level = actualLevel}
    (SpecShared {topTeam = expectedTopTeam, botTeam = expectedBotTeam, level = expectedLevel}) =
      actualTopTeam == expectedTopTeam
        && actualBotTeam == expectedBotTeam
        && actualLevel == expectedLevel

instance Satisfies EqSpec where
  satisfies
    (Balance.Result {topWins = actualTopWins, botWins = actualBotWins})
    (EqSpec {..}) =
      actualTopWins == topWins && actualBotWins == botWins

instance Satisfies LeqSpec where
  satisfies
    (Balance.Result {topWins = actualTopWins, botWins = actualBotWins})
    (LeqSpec {..}) =
      actualTopWins <= topMaxWins && botMinWins <= actualBotWins

-- | A spec specifies an expectation on the balance. 'SpecShared'
-- contains the data shared by all specs kinds.
data SpecShared = SpecShared
  { -- | The 'Team' in the top part
    topTeam :: Team,
    -- | The 'Team' in the bottom part
    botTeam :: Team,
    -- | The 'Level' being consider
    level :: Campaign.Level
  }

data EqSpec = EqSpec
  { -- | The exact expected number of the top team
    topWins :: Int,
    -- | The exact expected number of the bottom team
    botWins :: Int
  }

-- | A spec with bounds
data LeqSpec = LeqSpec
  { -- | The upper bound (included) of the number of wins of the top team
    topMaxWins :: Int,
    -- | The lower bound (included) of the number of wins of the bottom team
    botMinWins :: Int
  }

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
  Nat ->
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
  Balance.Result
play shareds level teams nbTurns =
  go shareds
    & map Match.matchResult
    & count (mkEmpty level $ fmap fst teams)
  where
    go (shared : rest) =
      let result = Match.play (Update.levelNGameModel AI.Easy shared level teams) nbTurns
       in traceShow (logString result) result : go rest
    go [] = []
    count :: Result -> [Match.MatchResult] -> Balance.Result
    count acc [] = acc
    count b@Balance.Result {draws} (Match.Draw : tail) = count b {draws = draws + 1} tail
    count tuple (Match.Error {} : tail) = count tuple tail -- not this test's business to fail on Error
    count b@Balance.Result {topWins} (Match.Win PlayerTop : tail) = count b {topWins = topWins + 1} tail
    count b@Balance.Result {botWins} (Match.Win PlayerBot : tail) =
      count b {botWins = botWins + 1} tail
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
