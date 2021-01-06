{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

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
  boardSetPart board pSpot $ start $ boardToPart board pSpot
