{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game
  ( PlayAction (..),
    play,
  )
where

import Board
import Card
import Data.Text (Text)

-- * This module contains the game mechanic i.e. the function
-- that takes a 'Board', a 'PlayAction', and returns an updated 'Board'

-- TODO smelc Use a type family to share this type with Update.PlayAction
data PlayAction
  = -- | Player puts a card from his hand on the board
    Place (Card Core) PlayerSpot CardSpot

play :: Board -> PlayAction -> Either Text Board
play board (Place card pSpot cSpot) = undefined
