{-# LANGUAGE RecordWildCards #-}

-- |
-- This module contains the debug commands that can be entered in dev mode.
-- It isin  the shared/ folder, for testing purpose: we don't want to compile
-- client/ in tests.
-- |
module Command where

import Card
import Data.Char
import Data.Function ((&))
import Data.Functor
import Data.List (find)

toLowerString :: String -> String
toLowerString = map toLower

data View
  = -- | Identifier of BuildView.
    Build
  deriving (Bounded, Enum)

allViews :: [View]
allViews = [minBound ..]

instance Show View where
  show Build = "build"

-- If you extend this datatype, extend 'allCommands' below
-- and 'getAllCommands' in SharedModel
data Command
  = -- | Command to obtain an extra card in the hand in GameView
    Gimme Card.ID
  | -- | Command to obtain mana
    GimmeMana
  | -- | Command to go to another view GameView
    Goto View

-- We could use SharedModel to restrict this list to members that
-- make sense at runtime, but dependency-wise, I don't want this module
-- to depend on SharedModel.
allCommands :: [Command]
allCommands =
  [Gimme $ Card.IDC (CreatureID kind team) [] | kind <- allCreatureKinds, team <- allTeams]
    ++ [Gimme $ Card.IDI item | item <- allItems]
    ++ [Gimme $ Card.IDN neutral | neutral <- allNeutrals]
    ++ [GimmeMana]
    ++ [Goto v | v <- allViews]

instance Show Command where
  show (Gimme (Card.IDC CreatureID {..} _)) =
    "gimme " ++ (show team & toLowerString) ++ " " ++ (show creatureKind & toLowerString)
  show (Gimme (Card.IDI item)) =
    "gimme " ++ (show item & toLowerString)
  show (Gimme (Card.IDN neutral)) =
    "gimme " ++ (show neutral & toLowerString)
  show GimmeMana =
    "gimme mana"
  show (Goto v) =
    "goto " ++ show v

class Read a where
  read :: String -> Maybe a

instance Command.Read Command where
  read s =
    allCommands
      & map (\c -> (c, show c))
      & find (\(_, s') -> s' == s)
      <&> fst
