module Pretty where

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
