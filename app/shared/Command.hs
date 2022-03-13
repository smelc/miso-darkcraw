{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- This module contains the debug commands that can be entered in dev mode.
-- It is in the shared/ folder, for testing purpose: we don't want to compile
-- client/ in tests.
-- |
module Command
  ( Command (..),
    Command.Read (..),
    allCommands,
  )
where

import qualified Campaign
import Card
import Data.Char
import Data.Function ((&))
import Data.Functor
import Data.List.Extra
import Spots

toLowerString :: String -> String
toLowerString = map toLower

-- If you extend this datatype, extend 'allCommands' below.
data Command
  = -- | Make the AI play at a spot
    AIPlay Spots.Player
  | -- | Command to trigger the 'assassins' event
    Assassins Spots.Player
  | -- | Command to end the current game. Parameter indicates how
    -- the playing player performed.
    EndGame Campaign.Outcome
  | -- | Command to trgger the 'fill the frontline' event
    FillTheFrontline Spots.Player
  | -- | Command to obtain an extra card in the hand in GameView
    Gimme Card.ID
  | -- | Command to obtain mana
    GimmeMana
  | -- | Command to trigger the 'king' event
    HailToTheKing Spots.Player
  | -- | Command to kill all creatures in a part
    Killall Spots.Player
  | -- | Command to reset the state of a part, as if it had started the
    -- game with the given team.
    Reboot Spots.Player Team

-- See 'Shared' for the version that uses content from 'Shared.Model'
-- to instantiate the parameter.
allCommands :: [CreatureID] -> [Command]
allCommands cids =
  [Gimme $ Card.IDC cid [] | cid <- sortBy compareCID cids]
    ++ [Gimme $ Card.IDI item | item <- sortOn show allItems]
    ++ [Gimme $ Card.IDN neutral | neutral <- sortOn show allNeutrals]
    ++ [AIPlay pSpot | pSpot <- [minBound ..]]
    ++ [EndGame r | r <- [minBound ..]]
    ++ [e pSpot | e <- [Assassins, FillTheFrontline, HailToTheKing, Killall], pSpot <- [minBound ..]]
    ++ [GimmeMana]
    ++ [Reboot pSpot t | pSpot <- [minBound ..], t <- allTeams]
  where
    compareCID
      CreatureID {creatureKind = ck1, team = t1}
      CreatureID {creatureKind = ck2, team = t2} =
        case compare t1 t2 of
          EQ -> compare ck1 ck2
          x -> x

instance Show Command where
  show =
    \case
      (AIPlay pSpot) -> "aiplay " ++ show pSpot
      (Assassins pSpot) -> "assassins " ++ show pSpot
      (EndGame r) ->
        ( case r of
            Campaign.Win -> "win"
            Campaign.Draw -> "draw"
            Campaign.Loss -> "lose"
        )
          ++ " game"
      (FillTheFrontline pSpot) -> "fill the " ++ show pSpot ++ " frontline"
      (Gimme (Card.IDC CreatureID {..} _)) ->
        "gimme " ++ (show' team) ++ " " ++ (show' creatureKind)
      (Gimme (Card.IDI item)) -> "gimme " ++ (show' item)
      (Gimme (Card.IDN neutral)) -> "gimme " ++ (show' neutral)
      GimmeMana -> "gimme mana"
      (HailToTheKing pSpot) -> "hail to the " ++ show pSpot ++ " king"
      (Killall pSpot) -> "killall " ++ show pSpot
      (Reboot pSpot team) -> "reboot " ++ show pSpot ++ " " ++ show' team
    where
      show' :: Show a => a -> String
      show' = toLowerString . show

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
