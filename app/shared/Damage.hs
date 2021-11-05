{-# LANGUAGE NamedFieldPuns #-}

-- | Module regarding damage, based on dice damage like in DnD
module Damage where

import Nat

data Damage = Damage
  { -- | The base damage amount. Damage dealt is equal or greater than this value.
    base :: Nat,
    -- | The variance added on top of 'base'. If '0', then damage is fixed.
    -- If greater than 0, a dice is rolled to add from '0' (included)
    -- to 'variance' (included) damage. So if you want '1-2' damage range,
    -- have 'base == 1' and 'variance == 1'.
    variance :: Nat
  }
  deriving (Bounded, Eq, Ord)

-- | 'a +^ n' augments the base damage of 'a' by 'n'
(+^) :: Damage -> Nat -> Damage
(+^) d 0 = d
(+^) d@(Damage {base}) n = d {base = base Prelude.+ n}

instance Show Damage where
  show Damage {base, variance}
    | variance == 0 = show base
    | otherwise = show base ++ "-" ++ show (base + variance)

instance Semigroup Damage where
  (<>) (Damage {base = b1, variance = v1}) (Damage {base = b2, variance = v2}) =
    Damage {base = b1 + b2, variance = v1 + v2}

instance Monoid Damage where
  mempty = Damage {base = 0, variance = 0}

-- | Whether something can deal damage. This function is meant to be used
-- qualified.
class Dealer a where
  dealer :: a -> Bool

instance Dealer Damage where
  dealer d = d /= mempty

instance Dealer a => Dealer (Maybe a) where
  dealer (Just a) = dealer a
  dealer Nothing = False
