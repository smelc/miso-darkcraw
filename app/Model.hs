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

-- | Type synonym for an application model
data Model = Model
  { board :: Board,
    uiCards :: [Card UI],
    handHover :: Maybe HandIndex
  }

deriving instance (Eq Model)
deriving instance (Generic Model)