{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- This module contains the debug commands that can be entered in dev mode.
-- It isin  the shared/ folder, for testing purpose: we don't want to compile
-- client/ in tests.
-- |
module Command
  ( Command (..),
    Command.Read (..),
    allCommands,
    PlayerSpot (..),
  )
where

import qualified Campaign
import Card
import Data.Char
import Data.Function ((&))
import Data.Functor
import Data.List (find)
import Data.List.Extra

toLowerString :: String -> String
toLowerString = map toLower

-- | We don't reuse 'Board' type, because that would create a cyclic
-- dependency
data PlayerSpot = Bot | Top
  deriving (Bounded, Enum)

instance Show PlayerSpot where
  show = \case Bot -> "bot"; Top -> "top"

-- If you extend this datatype, extend 'allCommands' below
-- and 'getAllCommands' in SharedModel
data Command
  = -- | Command to end the current game. Parameter indicates how
    -- the playing player performed.
    EndGame Campaign.Outcome
  | -- | Command to trgger the 'fill the frontline' event
    FillTheFrontline PlayerSpot
  | -- | Command to obtain an extra card in the hand in GameView
    Gimme Card.ID
  | -- | Command to obtain mana
    GimmeMana

-- See 'SharedModel' for the version that uses content from 'SharedModel'
-- to instantiate the parameter.
allCommands :: [CreatureID] -> [Command]
allCommands cids =
  [Gimme $ Card.IDC cid [] | cid <- sortBy compareCID cids]
    ++ [Gimme $ Card.IDI item | item <- sortOn show allItems]
    ++ [Gimme $ Card.IDN neutral | neutral <- sortOn show allNeutrals]
    ++ [EndGame r | r <- [minBound ..]]
    ++ [FillTheFrontline pSpot | pSpot <- [minBound ..]]
    ++ [GimmeMana]
  where
    compareCID
      CreatureID {creatureKind = ck1, team = t1}
      CreatureID {creatureKind = ck2, team = t2} =
        case compare t1 t2 of
          EQ -> compare ck1 ck2
          x -> x

instance Show Command where
  show (EndGame r) =
    ( case r of
        Campaign.Win -> "win"
        Campaign.Draw -> "draw"
        Campaign.Loss -> "lose"
    )
      ++ " game"
  show (FillTheFrontline pSpot) =
    "fill the " ++ show pSpot ++ " frontline"
  show (Gimme (Card.IDC CreatureID {..} _)) =
    "gimme " ++ (show team & toLowerString) ++ " " ++ (show creatureKind & toLowerString)
  show (Gimme (Card.IDI item)) =
    "gimme " ++ (show item & toLowerString)
  show (Gimme (Card.IDN neutral)) =
    "gimme " ++ (show neutral & toLowerString)
  show GimmeMana =
    "gimme mana"

class Read a where
  read :: String -> Maybe a

instance Command.Read Command where
  read s =
    allCommands cids
      & map (\c -> (c, show c))
      & find (\(_, s') -> s' == s)
      <&> fst
    where
      cids = [CreatureID kind team | kind <- allCreatureKinds, team <- allTeams]
