{-# LANGUAGE DeriveGeneric #-}

module Turn
  ( initial,
    next,
    Turn,
    toNat,
    toPlayerSpot,
  )
where

import GHC.Generics (Generic)
import Nat
import Spots

newtype Turn = Turn (Nat, Spots.Player)
  deriving (Eq, Generic, Show)

initial :: Turn
initial = Turn (1, startingPlayerSpot)

next :: Turn -> Turn
next (Turn (i, pSpot))
  | pSpot == startingPlayerSpot = Turn (i, endingPlayerSpot)
next (Turn (i, _)) = Turn (i + 1, startingPlayerSpot)

toNat :: Turn -> Nat
toNat (Turn (n, _)) = n

toPlayerSpot :: Turn -> Spots.Player
toPlayerSpot (Turn (_, pSpot)) = pSpot
