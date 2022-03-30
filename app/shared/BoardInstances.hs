{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module contains instances of classes regarding types defined
-- in 'Board'.
-- |
module BoardInstances where

import qualified Board
import Card
import CardInstances
import Constants
import Data.Foldable
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Nat
import qualified Skill
import Spots

instance Startable (Board.PlayerPart 'Core) where
  start Board.PlayerPart {..} =
    Board.PlayerPart {inPlace = Map.map start inPlace, mana = mana + initialMana + extraMana, ..}
    where
      extraMana =
        foldr'
          (\(cSpot, c@Creature {skills}) acc -> acc + sumSources skills + gaiaCloak cSpot c)
          (0 :: Nat)
          (Map.toList inPlace)
      sumSources [] = 0
      sumSources (Skill.Source (n, _) : skills) = n + sumSources skills -- Not inspecting
      -- the flag value, it's anyway simultaneously set to False.
      sumSources (_ : skills) = sumSources skills
      gaiaCloak cSpot Creature {items} =
        case Map.lookup cSpot deco == Just Board.Forest of
          True -> filter ((==) CloakOfGaia) items & natLength
          False -> 0

boardStart :: Board.T 'Core -> Spots.Player -> Board.T 'Core
boardStart board pSpot =
  Board.setPart board pSpot $ start $ Board.toPart board pSpot

class Stupid a where
  isStupid :: a -> Spots.Player -> Spots.Card -> Bool

instance Stupid Skill.State where
  isStupid s _ _ = Skill.isStupid s

instance Stupid (Creature 'Core) where
  isStupid Creature {skills} _ _ = any Skill.isStupid skills

instance Stupid (Board.T 'Core) where
  isStupid b pSpot cSpot =
    case Board.toInPlaceCreature b pSpot cSpot of
      Nothing -> False
      Just c -> BoardInstances.isStupid c pSpot cSpot
