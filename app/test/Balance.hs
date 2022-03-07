{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This module tests the balance
-- |
module Balance where

import Board
import qualified Campaign
import Card
import qualified Constants
import qualified Data.ByteString as BS
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List
import qualified Data.List.Index as IList
import Data.Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import Debug.Trace
import GHC.IO (unsafePerformIO)
import qualified Match
import qualified Model (Game (..))
import Nat
import SharedModel (SharedModel)
import qualified SharedModel
import Spots hiding (Card)
import Test.Hspec
import qualified Update
import qualified Weight

main :: SharedModel -> Bool -> SpecWith ()
main shared update =
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
    it "Endomatches balance is as expected" $ do
      check Human Campaign.Level0 Human
      check Evil Campaign.Level0 Evil
      check Undead Campaign.Level0 Undead
  where
    check team level opponent =
      shouldBe True $
        case (actual == expected, update) of
          (True, _) -> traceShow (prefix ++ details team actual opponent) True
          (False, False) -> traceShow (prefix ++ show expected ++ " is WRONG. Witnessed: " ++ show actual) False
          (False, True) ->
            if unsafePerformIO $ updateWeight team level opponent actual
              then traceShow (prefix ++ show expected ++ " has been UPDATED to " ++ show actual) True
              else traceShow (prefix ++ show expected ++ " could NOT BE UPDATED to " ++ show actual) False
      where
        actual = Balance.playAll (mkShareds nbMatches) team level nbTurns opponent & mconcat
        prefix = show team ++ "/" ++ show opponent ++ " matchup at " ++ show level ++ ": "
        expected = Weight.find team level opponent & fromJust & (\(i, j, k) -> Stat i j k)
    seeds = [0, 31 ..]
    mkShareds n = take n seeds & map (SharedModel.withSeed shared)
    nbMatches = 64
    nbTurns :: Nat = 8
    balances = Weight.balances & map (\(x, y, z, _, _, _) -> (x, y, z))

-- | @updateWeight t1 level t2 stat@ updates 'Weight' in place so that it contains
-- @stat@ for the matchup @t1 t2@ at the given @level@. The returned 'Bool'
-- indicates whether the update succeeded.
updateWeight :: Team -> Campaign.Level -> Team -> Stat -> IO Bool
updateWeight t1 level t2 Stat {..} = do
  content :: [Text] <- BS.readFile path <&> Data.Text.Encoding.decodeUtf8 <&> Text.lines
  let matchingLine = IList.ifind (\_ line -> match toMatch line) content
  case matchingLine of
    Nothing -> return False
    Just (i, matchingLine) -> do
      let newLine = "    (" <> tShow t1 <> ", Campaign." <> tShow level <> ", " <> tShow t2 <> ", " <> statContent <> ")"
          newLine' = newLine <> (if matchingLine `contains` ")," then "," else "")
          newContent :: [Text] = IList.setAt i newLine' content
      BS.writeFile path (Data.Text.Encoding.encodeUtf8 (Text.unlines newContent))
      return True
  where
    -- Whether a line (the second parameter) contains all strings in
    -- from the first parameter. For example
    -- @match ["Human", "Undead"] matches "Human noise Undead rest". This function could
    -- be implemented by a regexp match instead but I didn't want an extra dependency for this.
    match :: [Text] -> Text.Text -> Bool
    match ts line =
      case ts of
        [] -> True
        t1 : _ | Text.length line < Text.length t1 -> False
        t1 : trest | t1 `Text.isPrefixOf` line -> match trest (Text.drop (Text.length t1) line)
        _ -> match ts (Text.drop 1 line)
    tShow :: Show a => a -> Text
    tShow = Text.pack . show
    toMatch = [tShow t1, tShow level, tShow t2]
    path = "test/Weight.hs"
    statContent :: Text = tShow topWins <> (", ") <> tShow botWins <> ", " <> tShow draws
    contains s1 s2
      | Text.length s1 < Text.length s2 = False
      | s2 `Text.isPrefixOf` s1 = True
      | otherwise = not (Text.null s1) && contains (Text.drop 1 s1) s2

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
      -- I tried setting AI to Hard once and it didn't change the outcome significantly,
      -- except that it was wayyyyy slower.
      let result = Match.play (Update.levelNGameModel Constants.Easy shared level journey teams) nbTurns
       in result : go rest
    go [] = []
    journey =
      Campaign.unsafeJourney
        level
        (Board.toData (Spots.other Spots.startingPlayerSpot) teams & fst)
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
