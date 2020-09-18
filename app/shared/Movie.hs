{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module containing concrete lists of scenes for lobbies.
-- A movie is a list of scenes, built from the smart operators
-- from 'Cinema'.
module Movie (welcomeMovie) where

import Card (CreatureID (..), CreatureKind (..), Team (..))
import Cinema

w0 :: Element
w0 = Actor 0 $ CreatureID General Human

w01 :: Element
w01 = Actor 2 $ CreatureID Spearman Human

w02 :: Element
w02 = Actor 2 $ CreatureID Archer Human

w1 :: Element
w1 = Actor 1 $ CreatureID Vampire Undead

welcomeMovie :: [Scene Diff]
welcomeMovie =
  map
    (while 10) -- 10 tenth of seconds: 1 second
    [ w0 =: at' ToRight 0 15 <> w1 =: at 20 11,
      w0 =: right <> w1 =: left,
      w0 =: right <> w0 =: tell "Come on guys!" <> w1 =: left,
      w0 =: right <> w0 =: shutup <> w01 =: at' ToRight 0 15 <> w1 =: tell "Fresh meat!",
      w0 =: right <> w01 =: right <> w02 =: at' ToRight 0 15,
      w1 =: shutup,
      w0 =: right <> w01 =: right <> w01 =: up <> w02 =: right,
      w0 =: right <> w01 =: right <> w02 =: right,
      w0 =: right <> w01 =: right <> w02 =: right,
      w1 =: tell "Intriguing.."
    ]
