{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Module dealing with mana
module Mana
  ( amount,
    Labeler (..),
    Mana (..),
    Mana.Read (..),
    (Mana.>=),
    (Mana.<=),
    (Mana.>),
    (Mana.<),
    Mana.show,
  )
where

import qualified Constants
import qualified Contains
import Nat
import Text.Read
import qualified Turn

-- | Different mana costs
data Mana
  = -- | Mana cost is constant
    Const Nat
  | -- | Mana cost equals the number of remaining turns
    RemainingNbOfTurns
  deriving (Show)

-- | A safe alternative to prelude's @Read@
class Read a where
  read :: String -> Maybe a

instance Mana.Read Mana where
  read s =
    case readEither s :: Either String Nat of
      Right n -> Just (Const n)
      Left _ | s == "remaining_turns" -> Just RemainingNbOfTurns
      Left _ -> Nothing

comp :: (Nat -> Nat -> Bool) -> Turn.Turn -> Mana -> Nat -> Bool
comp op t m n = amount t m `op` n

-- | @>= t m n@ returns whether @m >= n@ when it's turn @t@
(>=) :: Turn.Turn -> Mana -> Nat -> Bool
(>=) = comp (Prelude.>=)

-- | @> t m n@ returns whether @m > n@ when it's turn @t@
(>) :: Turn.Turn -> Mana -> Nat -> Bool
(>) = comp (Prelude.>)

-- | @<= t m n@ returns whether @m <= n@ when it's turn @t@
(<=) :: Turn.Turn -> Mana -> Nat -> Bool
(<=) = comp (Prelude.<=)

-- | @< t m n@ returns whether @m < n@ when it's turn @t@
(<) :: Turn.Turn -> Mana -> Nat -> Bool
(<) = comp (Prelude.<)

-- | @amount t m@ returns the amount of mana of @m@ at turn @t@
amount :: Turn.Turn -> Mana -> Nat
amount t = \case
  Const n -> n
  RemainingNbOfTurns -> Constants.nbTurns `minusNatClamped` (Turn.toNat t)

-- | The text to show for mana, depending on whether the turn is provided or not
show :: Maybe Turn.Turn -> Mana -> String
show t m =
  case (t, m) of
    (Just t, m) -> Prelude.show $ amount t m
    (Nothing, Const n) -> Prelude.show n
    (Nothing, RemainingNbOfTurns) -> "?"

-- | How to typeset mana when an 'a' is available
class Labeler a where
  labeler :: a -> Mana -> String

instance Contains.Contains a Turn.Turn => Labeler a where
  labeler a =
    let turn :: Turn.Turn = Contains.to a
     in Mana.show (Just turn)
