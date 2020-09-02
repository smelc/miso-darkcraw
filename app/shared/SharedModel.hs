{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- This module contains the subset of the model that is needed by 'Game'
-- I didn't want to make 'Game' depend on 'Model', because it seemed overkill
-- and dangerous cyclewise in the long run. Hence I introduced this module.
-- |
module SharedModel
  ( identToCard,
    SharedModel (..),
    unsafeIdentToCard,
  )
where

import Card (Card (..), CardIdentifier (..), CardIdentifier, Creature (..), Creature, Phase (..))
import Data.Foldable (asum)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import System.Random
import Data.Function ((&))

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

identToCard :: SharedModel -> CardIdentifier -> Maybe (Card UI)
identToCard SharedModel {sharedCards} cid =
  asum $ map (identToCard' cid) sharedCards

unsafeIdentToCard :: SharedModel -> CardIdentifier -> Card UI
unsafeIdentToCard smodel ci = identToCard smodel ci & fromJust

identToCard' :: CardIdentifier -> Card p -> Maybe (Card p)
identToCard' cid card =
  case (cid, card) of
    (IDC cid2, CreatureCard Creature {creatureId = cid1}) | cid1 == cid2 -> Just card
    (IDN n2, NeutralCard n1) | n1 == n2 -> Just card
    (IDI i2, ItemCard i1) | i1 == i2 -> Just card
    _ -> Nothing
