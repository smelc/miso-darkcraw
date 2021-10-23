{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Pretty where

import qualified Art
import Board
import Card
import qualified Data.Text.Lazy as T
import Test.QuickCheck
import Text.Pretty.Simple (pShowNoColor)

newtype Pretty a = Pretty a
  deriving (Eq, Ord)

instance Show a => Show (Pretty a) where
  show (Pretty x) = T.unpack (pShowNoColor x)

instance Arbitrary a => Arbitrary (Pretty a) where
  arbitrary = Pretty <$> arbitrary
  shrink (Pretty x) = map Pretty (shrink x)

-- /!\ The instances that follow are easier to read but hide
-- some info, beware! /!\

instance {-# OVERLAPPING #-} Show (Pretty (Board 'Core)) where
  show (Pretty b) = Art.toASCII b

instance {-# OVERLAPPING #-} Show (Pretty (Maybe (Board 'Core))) where
  show (Pretty Nothing) = "Nothing"
  show (Pretty (Just b)) = Art.toASCII b
