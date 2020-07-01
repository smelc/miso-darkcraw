{-# LANGUAGE DeriveGeneric #-}

module Turn
  ( initialTurn,
    nextTurn,
    Turn,
    turnToInt,
    turnToPlayerSpot,
  )
where

import Board (PlayerSpot (..), playingPlayerSpot)
import GHC.Generics (Generic)

newtype Turn = Turn (Int, PlayerSpot)
  deriving (Eq, Generic, Show)

initialTurn :: Turn
initialTurn = Turn (1, playingPlayerSpot)

nextTurn :: Turn -> Turn
nextTurn (Turn (i, pSpot)) | pSpot == playingPlayerSpot = Turn (i, PlayerTop)
nextTurn (Turn (i, _)) = Turn (i + 1, playingPlayerSpot)

turnToInt :: Turn -> Int
turnToInt (Turn (i, _)) = i

turnToPlayerSpot :: Turn -> PlayerSpot
turnToPlayerSpot (Turn (_, pSpot)) = pSpot
