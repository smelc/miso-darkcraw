{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module containing concrete lists of scenes for lobbies.
-- A movie is a list of scenes, built from the smart operators
-- from 'Cinema'.
module Movie where

import Card (CreatureID (..), CreatureKind (..), Team (..))
import Cinema

w0 :: Element
w0 = Actor 0 $ CreatureID General Human

w1 :: Element
w1 = Actor 1 $ CreatureID Vampire Undead

welcomeMovie :: [Scene Diff]
welcomeMovie =
  map
    (while 1)
    [ w0 =: at 0 18 <~> w1 =: at 16 14,
      w0 =: right <~> w1 =: left,
      w0 =: right <~> w1 =: left,
      w1 =: tell "Haha fresh meat"
    ]
