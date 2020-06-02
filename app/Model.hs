{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Model where

import Board
import Card
import GHC.Generics

newtype HandIndex = HandIndex { unHandIndex :: Int }
  deriving (Eq, Show, Generic, Enum)

data HandFiddle
  = -- | Card in hand being hovered
    HandHovering HandIndex
  | -- | Card in hand being dragged to (x, y)
    HandDragging HandIndex
  deriving (Eq, Show, Generic)

-- | Type synonym for an application model
data Model = Model
  { board :: Board,
    uiCards :: [Card UI],
    handFiddle :: Maybe HandFiddle
  }

deriving instance (Eq Model)

deriving instance (Generic Model)
