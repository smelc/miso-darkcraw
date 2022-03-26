{-# LANGUAGE DeriveGeneric #-}

module Turn
  ( initial,
    next,
    setSpot,
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
  | otherwise = Turn (i + 1, startingPlayerSpot)

-- | Changes the spot in a 'Turn'
setSpot :: Spots.Player -> Turn -> Turn
setSpot spot t@(Turn (n, existing))
  | spot == existing = t
  | otherwise = Turn (n, spot)

toNat :: Turn -> Nat
toNat (Turn (n, _)) = n

toPlayerSpot :: Turn -> Spots.Player
toPlayerSpot (Turn (_, pSpot)) = pSpot
