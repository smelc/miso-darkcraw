{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Pretty where

import qualified Art
import qualified Board
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

instance {-# OVERLAPPING #-} Show a => Show (Pretty (Maybe a)) where
  show =
    \case
      Pretty Nothing -> "Nothing"
      Pretty (Just a) -> show a

-- /!\ The instance that follows is easier to read but hide
-- some info, beware! /!\

instance {-# OVERLAPPING #-} Show (Pretty (Board.T 'Core)) where
  show (Pretty b) = Art.toASCII b
