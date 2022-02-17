-- |
-- This module defines functions to deal with 'Word16'
-- |
module Nat where

import Data.Word

type Nat = Word16

intToNat :: Int -> Nat
intToNat n | (n < 0 || n > 65535) = error $ "intToNat: invalid value received: " ++ show n
intToNat n = toEnum n

natToInt :: Nat -> Int
natToInt = fromEnum

-- | Transforms an 'Int' into a 'Natural'. Returns 0 if the input 'Int'
-- is negative.
intToClampedNat :: Int -> Nat
intToClampedNat i | i < 0 = 0
intToClampedNat i = toEnum i

natLength :: [a] -> Nat
natLength [] = 0
natLength (_ : rest) = 1 + natLength rest

-- | Negate a natural. The returned value is less or equal to 0.
negate :: Nat -> Int
negate n = -(natToInt n)

-- | minusNatClamped n m returns n - m if n >= m, otherwise it returns 0.
minusNatClamped :: Nat -> Nat -> Nat
minusNatClamped n m | n >= m = n - m
minusNatClamped _ _ = 0
