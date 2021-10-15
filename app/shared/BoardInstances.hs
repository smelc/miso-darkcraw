{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module contains instances of classes regarding types defined
-- in 'Board'.
-- |
module BoardInstances where

import Board
import Card
import CardInstances
import Constants
import Data.Foldable
import qualified Data.Map.Strict as Map
import Nat
import qualified Skill

instance Startable (PlayerPart 'Core) where
  start PlayerPart {..} =
    PlayerPart {inPlace = Map.map start inPlace, mana = initialMana + extraMana, ..}
    where
      extraMana = foldr' (\Creature {skills} acc -> acc + sumSources skills) (0 :: Nat) inPlace
      sumSources [] = 0
      sumSources (Skill.Source' n _ : skills) = n + sumSources skills -- Not inspecting
      -- the flag value, it's anyway simultaneously set to False.
      sumSources (_ : skills) = sumSources skills

boardStart :: Board 'Core -> PlayerSpot -> Board 'Core
boardStart board pSpot =
  Board.setPart board pSpot $ start $ Board.toPart board pSpot

class Stupid a where
  isStupid :: a -> PlayerSpot -> CardSpot -> Bool

instance Stupid Skill.SkillCore where
  isStupid s _ _ = Skill.isStupid s

instance Stupid (Creature 'Core) where
  isStupid Creature {skills} _ _ = any Skill.isStupid skills

instance Stupid (Board 'Core) where
  isStupid b pSpot cSpot =
    case Board.toInPlaceCreature b pSpot cSpot of
      Nothing -> False
      Just c -> BoardInstances.isStupid c pSpot cSpot
