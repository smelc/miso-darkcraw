{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module containing concrete lists of scenes for lobbies.
-- A movie is a list of scenes, built from the smart operators
-- from 'Cinema'.
module Movie (welcomeMovie) where

import Card (CreatureID (..), CreatureKind (..), Team (..))
import Cinema
import Constants
import Tile

w0 :: Element
w0 = Actor 0 $ CreatureID General Human

w01 :: Element
w01 = Actor 2 $ CreatureID Spearman Human

w02 :: Element
w02 = Actor 3 $ CreatureID Archer Human

allw0right :: Frame ActorChange
allw0right = w0 =: right <> w01 =: right <> w02 =: right

w1 :: Element
w1 = Actor 1 $ CreatureID Vampire Undead

w10 :: Element
w10 = Actor 4 $ CreatureID Skeleton Undead

whiteAppears :: Int -> Int -> [Frame ActorChange]
whiteAppears x y =
  map f [WhiteAppears0, WhiteAppears1, WhiteAppears2, WhiteAppears3, WhiteAppears4]
  where
    f tile = TileElement tile =: at x y

welcomeGhostMovie1 :: Scene ActorChange
welcomeGhostMovie1 =
  mconcat
    [ while 9 $ g =: at' ToRight 1 0,
      while 8 $ g =: down,
      while 15 $ g =: right,
      while 8 $ g =: down,
      while 12 $ g =: right,
      while 7 $ g =: down,
      while 8 $ g =: right,
      while 10 $ g =: down,
      while 20 $ g =: down,
      while 15 $ g =: right,
      while 8 $ g =: right,
      while 15 $ g =: right
    ]
  where
    g = Actor 5 $ CreatureID Ghost Undead

welcomeGhostMovie2 :: Scene ActorChange
welcomeGhostMovie2 =
  mconcat
    [ while 15 $ g =: at (lobbiesCellWidth - 3) 0,
      while 10 $ g =: down,
      while 12 $ g =: left,
      while 18 $ g =: down,
      while 12 $ g =: left,
      while 7 $ g =: down,
      while 8 $ g =: left,
      while 10 $ g =: left,
      while 10 $ g =: up,
      while 14 $ g =: up,
      while 20 $ g =: up,
      while 15 $ g =: left,
      while 8 $ g =: right,
      while 15 $ g =: right
    ]
  where
    g = Actor 6 $ CreatureID Ghost Undead

welcomeFightMovie :: Scene ActorChange
welcomeFightMovie =
  foldMap
    (while 10) -- 10 tenth of seconds: 1 second
    [ w0 =: at' ToRight 0 15 <> w1 =: at (lobbiesCellWidth - 1) 11,
      w0 =: right <> w1 =: left,
      w0 =: right <> w0 =: tell "Come on guys!" <> w1 =: left,
      w0 =: right <> w0 =: shutup <> w01 =: at' ToRight 0 15 <> w1 =: tell "Fresh meat!",
      w0 =: right <> w01 =: right <> w02 =: at' ToRight 0 15,
      w1 =: shutup,
      w0 =: right <> w01 =: right <> w01 =: up <> w02 =: right,
      allw0right,
      allw0right,
      allw0right,
      allw0right <> w1 =: left,
      allw0right <> w1 =: left,
      allw0right <> w1 =: left,
      allw0right <> w1 =: left,
      w0 =: up <> w01 =: right <> w02 =: right,
      w0 =: up <> w01 =: right <> w01 =: up <> w02 =: right <> w02 =: up
    ]
    <> while 5 (w1 =: tell "iugp9b7")
    <> while 1 (w1 =: shutup)
    <> foldMap (while 2) (whiteAppears 12 11)
    <> while 10 (w10 =: at 12 11)

welcomeMovie :: Scene ActorChange
welcomeMovie = welcomeGhostMovie1 ||| welcomeGhostMovie2 ||| welcomeFightMovie
