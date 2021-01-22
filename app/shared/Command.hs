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

-- If you extend this datatype, extend 'allCommands' too
data Command = Gimme CreatureID

-- We could use SharedModel to restrict this list to members that
-- make sense at runtime, but dependency-wise, I don't want this module
-- to depend on SharedModel.
allCommands =
  [Gimme $ CreatureID kind team | kind <- allCreatureKinds, team <- allTeams]

instance Show Command where
  show (Gimme CreatureID {..}) =
    "gimme " ++ (show team & toLowerString) ++ " " ++ (show creatureKind & toLowerString)

class Read a where
  read :: String -> Maybe a

instance Command.Read Command where
  read s =
    allCommands
      & map (\c -> (c, show c))
      & filter (\(_, s') -> s' == s)
      & listToMaybe <&> fst
