{-# LANGUAGE DeriveGeneric #-}

module Turn
  ( initial,
    next,
    setSpot,
    T,
    toNat,
    toPlayerSpot,
  )
where

import GHC.Generics (Generic)
import Nat
import Spots

newtype T = T (Nat, Spots.Player)
  deriving (Eq, Generic, Ord, Show)

initial :: T
initial = T (1, startingPlayerSpot)

next :: T -> T
next (T (i, pSpot))
  | pSpot == startingPlayerSpot = T (i, endingPlayerSpot)
  | otherwise = T (i + 1, startingPlayerSpot)

-- | Changes the spot in a 'T'
setSpot :: Spots.Player -> T -> T
setSpot spot t@(T (n, existing))
  | spot == existing = t
  | otherwise = T (n, spot)

toNat :: T -> Nat
toNat (T (n, _)) = n

toPlayerSpot :: T -> Spots.Player
toPlayerSpot (T (_, pSpot)) = pSpot
