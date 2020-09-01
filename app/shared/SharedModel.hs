{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- This module contains the subset of the model that is needed by 'Game'
-- I didn't want to make 'Game' depend on 'Model', because it seemed overkill
-- and dangerous cyclewise in the long run. Hence I introduced this module.
-- |
module SharedModel where

import Card (Card (..), Phase (..))
import GHC.Generics (Generic)
import System.Random

instance Eq StdGen where
  std1 == std2 = show std1 == show std2

-- | The part of the model that is likely to be used by all pages
-- | i.e. all possible models
data SharedModel = SharedModel
  { -- | Data obtained at load time, that never changes
    sharedCards :: [Card UI],
    -- | RNG obtained at load time, to be user whenever randomness is needed
    sharedStdGen :: StdGen
  }
  deriving (Eq, Generic, Show)
