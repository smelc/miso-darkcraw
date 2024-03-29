{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- This module simulates playing an entire game
-- |
module Match
  ( main,
    MatchResult (..),
    mkTestGame,
    play,
    Result (..),
    testStupidity,
  )
where

import qualified Board
import Card
import qualified Constants (Difficulty (..))
import qualified Contains
import Control.Monad.Except
import Damage (Damage (..))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace (trace, traceShow)
import qualified Direction
import Generators ()
import Model
import qualified Move
import Nat
import qualified Network
import qualified Shared
import Spots
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck ((==>))
import qualified Theme
import qualified Turn
import Update

main :: Shared.Model -> SpecWith ()
main shared = do
  describe "Playing a match doesn't return Error" $
    it "forall t1, t2 :: Team, play shared t1 t2 8 isn't Error" $
      all f teamProduct
  testStupidity shared
  where
    f (t1, t2) =
      map
        (\seed -> play (model seed) 8)
        [1 .. 48]
        & not . any (isError . matchResult . traceResult)
      where
        model seed =
          mkTestGame (Shared.withSeed shared seed) Constants.Easy (Board.Teams t1' t2')
        (t1', t2') = ((t1, Shared.getInitialDeck shared t1), (t2, Shared.getInitialDeck shared t2))
    isError (Error msg) = traceShow msg True
    isError Draw = False
    isError (Win _) = False
    teamProduct = [(t1, t2) | t1 <- allTeams, t2 <- allTeams]
    traceResult r =
      case (matchResult r, last $ (map board . models) r) of
        (Error msg, _) -> traceShow ("[ERR] " ++ Text.unpack msg) r
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
        sign (Just (board :: Board.T 'Core)) =
          case compare (Board.getpk @'Board.Score PlayerTop board) (Board.getpk @'Board.Score PlayerBot board) of
            LT -> "<"
            EQ -> "="
            GT -> ">"
        sign Nothing = "?"
        showScore Nothing _ = "?"
        showScore (Just (board :: Board.T 'Core)) pSpot =
          show (Board.toPart board pSpot & Board.team)
            ++ " "
            ++ show (Board.getpk @'Board.Score pSpot board)

-- | An instance of 'Model.Game' that is OK for testing.
mkTestGame ::
  Shared.Model ->
  Constants.Difficulty ->
  -- | The teams
  Board.Teams (Team, [Card.Card 'Core]) ->
  Model.Game
mkTestGame shared difficulty teams =
  Model.mkInitialGame shared difficulty Theme.Forest mempty encounter Nothing teams
  where
    (opponent, _) = Board.toData (Spots.other Spots.startingPlayerSpot) teams
    encounter = (Direction.Coord (0, 0), Network.Fight opponent Theme.Forest)

testStupidity :: Shared.Model -> SpecWith ()
testStupidity shared =
  describe "Stupidity is handled correctly" $
    prop "the score is correctly predicted" $
      \(cSpot, topTeam, seed) ->
        not (inTheBack cSpot)
          ==> let teams = Board.Teams topTeam Human
                  board = initialBoard shared teams cSpot
                  model = Update.unsafeInitialGameModel Constants.Easy (mkShared seed) (mkTeamData teams) board
               in play model 8 `shouldSatisfy` isValid
  where
    initialBoard s teams cSpot = Board.small s teams ogreID [] startingPlayerSpot cSpot
    ogreID = CreatureID Card.Ogre Human
    ogreSpot = PlayerBot
    Damage {base = ogreAttack} = Shared.idToCreature shared ogreID [] & fromJust & attack
    mkShared seed = Shared.withSeed shared seed
    mkTeamData teams = teams <&> (\t -> (t, Shared.getInitialDeck shared t))
    isValid Result {models} = all isValidModel models
    isValidModel Model.Game {board, turn} =
      expectedScore == score
        || trace (show expectedScore ++ "<>" ++ show score ++ " at turn " ++ show turn) False
      where
        score = Board.getpk @'Board.Score ogreSpot board
        turni = Turn.toNat turn
        pSpot = Turn.toPlayerSpot turn
        stupidFreq = 4
        nbAttacks played = (turni - (if played then 0 else 1)) - ((turni - 1) `div` stupidFreq)
        expectedScore = nbAttacks (pSpot /= ogreSpot) * ogreAttack

data MatchResult = Draw | Error Text | Win Spots.Player
  deriving (Show)

data Result = Result
  { models :: [Model.Game],
    matchResult :: MatchResult
  }
  deriving (Show)

play ::
  Model.Game ->
  -- The number of turns
  Nat ->
  Result
play model nbTurns =
  go model []
  where
    go m@Model.Game {turn} models
      | Turn.toNat turn > nbTurns =
          Result (reverse models) (toMatchResult m)
    go m models =
      case playOneTurn m of
        Left msg -> Result (reverse models) (Error msg)
        Right m' -> go m' $ m : models

toMatchResult :: Model.Game -> MatchResult
toMatchResult Model.Game {board}
  | scoreTop == scoreBot = Draw
  | scoreTop > scoreBot = Win PlayerTop
  | otherwise = Win PlayerBot
  where
    score pSpot = Board.toPart board pSpot & Board.score
    (scoreTop, scoreBot) = (score PlayerTop, score PlayerBot)

-- | Play one turn. XXX: return the intermediate 'GameModel's, because they
-- make sense for being checked with properties from 'Invariants'.
playOneTurn :: MonadError Text m => a ~ Model.Game => a -> m a
playOneTurn m = Contains.onContainedE playOneTurnK m

-- | The signature @Move.Kernel ()@ is important. The @()@ instance triggers
-- the 'simulation' case (see 'Move').
playOneTurnK :: MonadError Text m => a ~ Move.Kernel () => a -> m a
playOneTurnK m = do
  m <- playPart startingPlayerSpot m
  playPart (Spots.other startingPlayerSpot) m
  where
    playPart pSpot m = do
      (m, nextSched) <- Move.startTurn Move.AI pSpot m
      Move.simRunAllMaybe nextSched m
