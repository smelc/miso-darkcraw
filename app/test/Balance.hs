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
import Data.Map.Strict (Map)
import Data.Map.Strict as Map (singleton, toList, unionWith)
import Data.Maybe
import qualified Data.Text as Text
import Debug.Trace
import GHC.Float
import qualified Match
import Model (GameModel (..))
import Nat
import SharedModel (SharedModel)
import qualified SharedModel
import Spots hiding (Card)
import Test.Hspec
import TestLib (shouldAllSatisfy)
import qualified Update

main :: SharedModel -> SpecWith ()
main shared =
  describe "Balance" $ do
    xit "The teams' balance is as expected" $ do
      -- FIXME @smelc replug me
      -- We only need to test one team with 'checkBalance' (here 'Human'),
      -- because 'checkBalance' iterates over enemy teams.
      checkBalance specs $ (Balance.playAll (mkShareds nbMatches) Human Campaign.Level0 nbTurns) & map snd
      checkBalance specs $ (Balance.playAll (mkShareds nbMatches) Human Campaign.Level1 nbTurns) & map snd
      -- 'checkSpecOfFight', however, is for a specific match up.
      checkSpecOfFight Human (ZKnights, zknightDeck) Campaign.Level1 zspecs
      checkSpecOfFight Undead (ZKnights, zknightDeck) Campaign.Level1 zspecs
    xit "Starting team doesn't have an advantage" $ do
      checkBalanceStart Undead Undead
      checkBalanceStart Human Human
  where
    checkBalance specs (results :: [Balance.Result]) =
      Map.toList (classify2 $ classify1 results specs) `shouldAllSatisfy` pred
    pred :: Show a => Satisfies a => (a, [Balance.Result]) -> Bool
    pred (spec, results) =
      go results && isTight
      where
        go [] = traceShow (show spec ++ " satisfied by " ++ show (length results) ++ " result" ++ plural results) True
        go (r : res)
          | satisfies r spec = go res
          | otherwise = traceShow (show r ++ " violates spec " ++ show spec) False
        isTight =
          case tight results spec of
            Wrong -> traceShow ("Unexpected case, satifies should have failed already") False
            Tight -> True
            Loose -> traceShow (show spec ++ " is not tight for the following results:\n  " ++ (concat $ intersperse "\n  " (map show results))) False
    -- Tests the balance of t1 against t2, at the given level
    checkSpecOfFight t1 p2@(_t2, _t2Deck) level specs =
      checkBalance specs (playOne (mkShareds nbMatches) t1 level p2 nbTurns)
    checkBalanceStart t1 t2 =
      let shareds = take nbEndoMatches seeds & map (SharedModel.withSeed shared)
       in Balance.play shareds Campaign.Level0 teams nbTurns `shouldSatisfy` checkSpec specs
      where
        teams = Teams t1 t2 <&> (\t -> (t, Card.teamDeck uiCards t))
        uiCards = SharedModel.getCards shared
    seeds = [0, 31 ..]
    mkShareds n = take n seeds & map (SharedModel.withSeed shared)
    (nbMatches, nbEndoMatches) = (64, nbMatches)
    nbTurns :: Nat = 8
    zknightDeck = SharedModel.getInitialDeck shared ZKnights
    checkSpec specs r@Balance.Result {..} =
      case find (satisfies r) specs of
        Nothing -> traceShow ("Unexpected spec: " ++ show r) False
        Just spec -> traceShow (show spec ++ " (" ++ show r ++ ")") True
    specs :: [Balance.Spec] = map Eq eqSpecs ++ map Leq leqSpecs
    zspecs :: [Balance.Spec] = map Leq zknights
    eqSpecs =
      -- Level0
      map
        (uncurry FullEqSpec)
        [ ( SpecShared {topTeam = Human, botTeam = Undead, level = Campaign.Level0},
            EqSpec {topWins = 43, botWins = 20}
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
            LeqSpec {topMaxWins = 45, botMinWins = 18}
          ),
          ( SpecShared {topTeam = Human, botTeam = Evil, level = Campaign.Level1},
            LeqSpec {topMaxWins = 49, botMinWins = 15}
          )
        ]
    zknights =
      map
        (uncurry FullLeqSpec)
        [ ( SpecShared {topTeam = Human, botTeam = ZKnights, level = Campaign.Level1},
            LeqSpec {topMaxWins = 31, botMinWins = 32}
          ),
          ( SpecShared {topTeam = Undead, botTeam = ZKnights, level = Campaign.Level1},
            LeqSpec {topMaxWins = 33, botMinWins = 31}
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
    plural l = (if length l == 1 then "" else "s")

-- | An expectation of the Balance
data Spec
  = Leq FullLeqSpec
  | Eq FullEqSpec
  deriving (Eq, Ord)

toSharedSpec :: Balance.Spec -> SpecShared
toSharedSpec = \case
  Leq (FullLeqSpec s _) -> s
  Eq (FullEqSpec s _) -> s

instance Show Balance.Spec where
  show (Leq spec) = show spec
  show (Eq spec) = show spec

-- | A pair of a 'SpecShared' and a 'EqSpec'
data FullEqSpec = FullEqSpec SpecShared EqSpec
  deriving (Eq, Ord)

instance Show FullEqSpec where
  show (FullEqSpec (SpecShared {topTeam, botTeam}) (EqSpec {topWins, botWins})) =
    show topTeam ++ " (top): " ++ show topWins ++ " wins, " ++ show botTeam
      ++ " (bot):"
      ++ show botWins
      ++ " wins"

-- | A pair of a 'SpecShared' and a 'LeqSpec'
data FullLeqSpec = FullLeqSpec SpecShared LeqSpec
  deriving (Eq, Ord)

instance Show FullLeqSpec where
  show (FullLeqSpec (SpecShared {topTeam, botTeam}) (LeqSpec {topMaxWins, botMinWins})) =
    show botMinWins ++ " <= " ++ show botTeam ++ " <= _, "
      ++ "_ <= "
      ++ show topTeam
      ++ " <= "
      ++ show topMaxWins

data Tightness
  = -- | 'satisfies r a' would return 'False'
    Wrong
  | -- | 'satisfies r a' would return 'True', but 'satisfies b' would
    -- also hold, for some 'b' stricter than 'a'.
    Loose
  | -- | 'satisfies r a' would return 'True' and there is no 'stricter' value
    -- 'b' for which 'satisfies r b' would hold too.
    Tight
  deriving (Show)

instance Monoid Tightness where
  mempty = Tight

instance Semigroup Tightness where
  (<>) Wrong _ = Wrong
  (<>) _ Wrong = Wrong
  (<>) Loose _ = Loose
  (<>) _ Loose = Loose
  (<>) Tight Tight = Tight

class Satisfies a where
  -- | Whether the given 'Balance.Result' satisfies 'a'
  satisfies :: Balance.Result -> a -> Bool

  -- | See doc of values of 'Tightness'
  tight :: [Balance.Result] -> a -> Tightness

instance Satisfies Balance.Spec where
  satisfies r (Leq (FullLeqSpec shared sub)) = satisfies r shared && satisfies r sub
  satisfies r (Eq (FullEqSpec shared sub)) = satisfies r shared && satisfies r sub

  tight rs (Leq (FullLeqSpec shared sub)) = tight rs shared <> tight rs sub
  tight rs (Eq (FullEqSpec shared sub)) = tight rs shared <> tight rs sub

instance Satisfies SpecShared where
  satisfies
    Balance.Result {topTeam = actualTopTeam, botTeam = actualBotTeam, level = actualLevel}
    (SpecShared {topTeam = expectedTopTeam, botTeam = expectedBotTeam, level = expectedLevel}) =
      actualTopTeam == expectedTopTeam
        && actualBotTeam == expectedBotTeam
        && actualLevel == expectedLevel
  tight rs a =
    if all (flip satisfies a) rs then Tight else Wrong

instance Satisfies EqSpec where
  satisfies
    (Balance.Result {topWins = actualTopWins, botWins = actualBotWins})
    (EqSpec {..}) =
      actualTopWins == topWins && actualBotWins == botWins
  tight rs a =
    if all (flip satisfies a) rs then Tight else Wrong

data LeqTightness
  = -- | Both min bound and max bound reached
    LeqTight
  | -- | Min bound was reached
    MinReached
  | -- | Max bound was reached
    MaxReached
  | -- | Neither min bound nor max bound was reached
    LeqLoose
  deriving (Eq)

instance Monoid LeqTightness where
  mempty = LeqLoose

instance Semigroup LeqTightness where
  (<>) LeqLoose b = b
  (<>) b LeqLoose = b
  (<>) LeqTight _ = LeqTight
  (<>) _ LeqTight = LeqTight
  (<>) MinReached MaxReached = LeqTight
  (<>) MaxReached MinReached = LeqTight
  (<>) MinReached MinReached = MinReached
  (<>) MaxReached MaxReached = MinReached

instance Satisfies LeqSpec where
  satisfies
    (Balance.Result {topWins = actualTopWins, botWins = actualBotWins})
    (LeqSpec {..}) =
      actualTopWins <= topMaxWins && botMinWins <= actualBotWins

  tight rs spec@(LeqSpec {topMaxWins, botMinWins}) =
    case rs of
      [] -> mempty -- Degenerated case
      _ ->
        let pairs :: [(Bool, LeqTightness)] = map one rs
         in if any not (map fst pairs)
              then Wrong
              else case mconcat $ map snd pairs of
                LeqLoose -> Loose
                MinReached -> Loose
                MaxReached -> Loose
                LeqTight -> Tight
    where
      one r@(Balance.Result {topWins = actualTopWins, botWins = actualBotWins})
        | actualTopWins == topMaxWins && actualBotWins == botMinWins = (True, LeqTight)
        | actualTopWins == topMaxWins = (True, MaxReached)
        | actualBotWins == botMinWins = (True, MinReached)
        | satisfies r spec = (True, LeqLoose)
        | otherwise = (False, LeqLoose)

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
  deriving (Eq, Ord)

data EqSpec = EqSpec
  { -- | The exact expected number of the top team
    topWins :: Int,
    -- | The exact expected number of the bottom team
    botWins :: Int
  }
  deriving (Eq, Ord)

-- | A spec with bounds
data LeqSpec = LeqSpec
  { -- | The upper bound (included) of the number of wins of the top team
    topMaxWins :: Int,
    -- | The lower bound (included) of the number of wins of the bottom team
    botMinWins :: Int
  }
  deriving (Eq, Ord)

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

-- | Given a list of results and a list of specs, returns a list of pairs
-- of a result and specs, such that only the specs in the pair can satisfy
-- this result. This partial satisfaction is obtained by only
-- checking for compatibility of the 'SpecShared' part of every 'Balance.Spec'.
classify1 :: [Balance.Result] -> [Balance.Spec] -> [(Balance.Result, [Balance.Spec])]
classify1 results specs =
  case results of
    [] -> []
    r : rs ->
      (r, rSpecs) : classify1 rs specs
      where
        rSpecs = Prelude.filter (\spec -> satisfies r (toSharedSpec spec)) specs

-- | Given the result of 'classify1', put it in shape for calling
-- 'satisfies' easily afterwards.
classify2 :: [(Balance.Result, [Balance.Spec])] -> Map Balance.Spec [Balance.Result]
classify2 pairs =
  case pairs of
    [] -> mempty
    (r, []) : _ -> error $ "No matching SpecShared found for " ++ show r ++ ". Did you forget a spec?"
    (r, [spec]) : rest -> Map.unionWith (++) (Map.singleton spec [r]) (classify2 rest)
    (r, specs) : _ ->
      error $
        "Too many matching SpecShared ("
          ++ show (length specs)
          ++ ") found for "
          ++ show r
          ++ ". Expected exactly one."

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
    otherTeams = [t | t <- Campaign.anywhere, t /= team]
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
  [Balance.Result]
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
  Balance.Result
play shareds level teams nbTurns =
  go shareds
    & map Match.matchResult
    & count (mkEmpty level $ fmap fst teams)
  where
    go (shared : rest) =
      let result = Match.play (Update.levelNGameModel AI.Easy shared level teams) nbTurns
       in result : go rest
    go [] = []
    count :: Result -> [Match.MatchResult] -> Balance.Result
    count acc [] = acc
    count b@Balance.Result {draws} (Match.Draw : tail) = count b {draws = draws + 1} tail
    count tuple (Match.Error {} : tail) = count tuple tail -- not this test's business to fail on Error
    count b@Balance.Result {topWins} (Match.Win PlayerTop : tail) = count b {topWins = topWins + 1} tail
    count b@Balance.Result {botWins} (Match.Win PlayerBot : tail) =
      count b {botWins = botWins + 1} tail
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
