{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module Model where

import Board
import Card

newtype HandIndex = HandIndex Int
  deriving (Eq, Show)

-- | Type synonym for an application model
data Model
  = Model
      { board :: Board,
        uiCards :: [Card UI],
        handHover :: Maybe HandIndex
      }

deriving instance (Eq Model)
