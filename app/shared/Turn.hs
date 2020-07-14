{-# LANGUAGE DeriveGeneric #-}

module Turn
  ( initialTurn,
    nextTurn,
    Turn,
    turnToInt,
    turnToPlayerSpot,
  )
where

import Board (PlayerSpot (..), endingPlayerSpot, startingPlayerSpot)
import GHC.Generics (Generic)

newtype Turn = Turn (Int, PlayerSpot)
  deriving (Eq, Generic, Show)

initialTurn :: Turn
initialTurn = Turn (1, startingPlayerSpot)

nextTurn :: Turn -> Turn
nextTurn (Turn (i, pSpot))
  | pSpot == startingPlayerSpot = Turn (i, endingPlayerSpot)
nextTurn (Turn (i, _)) = Turn (i + 1, startingPlayerSpot)

turnToInt :: Turn -> Int
turnToInt (Turn (i, _)) = i

turnToPlayerSpot :: Turn -> PlayerSpot
turnToPlayerSpot (Turn (_, pSpot)) = pSpot
