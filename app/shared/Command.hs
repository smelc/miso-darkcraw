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
import Data.Maybe

toLowerString = map toLower

-- If you change the first member of this type, change 'allViews' too
data View
  = -- | Identifier of BuildView.
    Build
  deriving (Enum)

allViews = [Build ..]

instance Show View where
  show Build = "build"

-- If you extend this datatype, extend 'allCommands' below
-- and 'getAllCommands' in SharedModel
data Command
  = -- | Command to obtain an extra card in the hand in GameView
    Gimme CreatureID
  | -- | Command to go to another view GameView
    Goto View

-- We could use SharedModel to restrict this list to members that
-- make sense at runtime, but dependency-wise, I don't want this module
-- to depend on SharedModel.
allCommands :: [Command]
allCommands =
  [Gimme $ CreatureID kind team | kind <- allCreatureKinds, team <- allTeams]
    ++ [Goto v | v <- allViews]

instance Show Command where
  show (Gimme CreatureID {..}) =
    "gimme " ++ (show team & toLowerString) ++ " " ++ (show creatureKind & toLowerString)
  show (Goto v) =
    "goto " ++ show v

class Read a where
  read :: String -> Maybe a

instance Command.Read Command where
  read s =
    allCommands
      & map (\c -> (c, show c))
      & filter (\(_, s') -> s' == s)
      & listToMaybe <&> fst
