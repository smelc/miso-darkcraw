{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module Model where

import Card

-- | Sum type for application events
data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  deriving (Show, Eq)

-- | Type synonym for an application model
newtype Model = Model { uiCards :: [Card UI] }

deriving instance (Eq Model)
