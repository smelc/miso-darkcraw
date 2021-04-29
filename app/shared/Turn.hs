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

-- TODO @smelc rename to 'initial' and use qualified
initialTurn :: Turn
initialTurn = Turn (1, startingPlayerSpot)

-- TODO @smelc rename to 'next' and use qualified
nextTurn :: Turn -> Turn
nextTurn (Turn (i, pSpot))
  | pSpot == startingPlayerSpot = Turn (i, endingPlayerSpot)
nextTurn (Turn (i, _)) = Turn (i + 1, startingPlayerSpot)

-- TODO @smelc rename to 'toInt' and use qualified
turnToInt :: Turn -> Int
turnToInt (Turn (i, _)) = i

-- TODO @smelc rename to 'toPlayerSpot' and use qualified
turnToPlayerSpot :: Turn -> PlayerSpot
turnToPlayerSpot (Turn (_, pSpot)) = pSpot
