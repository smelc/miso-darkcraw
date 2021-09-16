{-# LANGUAGE DeriveGeneric #-}

module Turn
  ( initial,
    next,
    Turn,
    toInt,
    toNat,
    toPlayerSpot,
  )
where

import Board (PlayerSpot (..), endingPlayerSpot, startingPlayerSpot)
import GHC.Generics (Generic)
import Nat

-- FIXME @smelc Int->Nat
newtype Turn = Turn (Int, PlayerSpot)
  deriving (Eq, Generic, Show)

initial :: Turn
initial = Turn (1, startingPlayerSpot)

next :: Turn -> Turn
next (Turn (i, pSpot))
  | pSpot == startingPlayerSpot = Turn (i, endingPlayerSpot)
next (Turn (i, _)) = Turn (i + 1, startingPlayerSpot)

toInt :: Turn -> Int
toInt (Turn (i, _)) = i

toNat :: Turn -> Nat
toNat = intToNat . toInt

toPlayerSpot :: Turn -> PlayerSpot
toPlayerSpot (Turn (_, pSpot)) = pSpot
