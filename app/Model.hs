{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module Model where

import Board
import Card

-- | Type synonym for an application model
data Model
  = Model
      { board :: Board,
        uiCards :: [Card UI],
        handHover :: Maybe Int
      }

deriving instance (Eq Model)
