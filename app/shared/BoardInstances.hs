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
import qualified Data.Map.Strict as Map

instance Startable (PlayerPart 'Core) where
  start PlayerPart {..} = PlayerPart {inPlace = Map.map start inPlace, ..}

boardStart :: Board 'Core -> PlayerSpot -> Board 'Core
boardStart board pSpot =
  Board.setPart board pSpot $ start $ Board.toPart board pSpot

class Stupid a where
  isStupid :: a -> PlayerSpot -> CardSpot -> Bool

instance Stupid SkillCore where
  isStupid s _ _ = Card.isStupid s

instance Stupid (Creature 'Core) where
  isStupid Creature {skills} _ _ = any Card.isStupid skills

instance Stupid (Board 'Core) where
  isStupid b pSpot cSpot =
    case Board.toInPlaceCreature b pSpot cSpot of
      Nothing -> False
      Just c -> BoardInstances.isStupid c pSpot cSpot
