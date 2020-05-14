{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module Model where

import Board
import Card

-- | Sum type for application events
data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  deriving (Show, Eq)

-- | Type synonym for an application model
data Model = Model {
  board :: Board,
  uiCards :: [Card UI]
}

deriving instance (Eq Model)
