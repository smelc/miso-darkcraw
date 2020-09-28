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

allRight :: [Element] -> Frame ActorChange
allRight actors = mconcat [a =: right | a <- actors]

whiteAppears :: Element -> Int -> Int -> [Frame ActorChange]
whiteAppears e x y =
  [e =: at (tileSprite WhiteAppears0) x y]
    ++ map f [WhiteAppears1, WhiteAppears2, WhiteAppears3, WhiteAppears4]
    ++ [e =: Leave]
  where
    f tile = e =: dress (tileSprite tile)

welcomeGhostMovie1 :: Scene ()
welcomeGhostMovie1 = do
  g <- newActor
  while 9 $ g =: at' (creatureSprite $ CreatureID Ghost Undead) ToRight 1 0
  while 8 $ g =: down
  while 15 $ g =: right
  while 8 $ g =: down
  while 12 $ g =: right
  while 7 $ g =: down
  while 8 $ g =: right
  while 10 $ g =: down
  while 20 $ g =: down
  while 15 $ g =: right
  while 8 $ g =: right
  while 15 $ g =: right

welcomeGhostMovie2 :: Scene ()
welcomeGhostMovie2 = do
  g <- newActor
  while 15 $ g =: at (creatureSprite $ CreatureID Ghost Undead) (lobbiesCellWidth - 3) 0
  while 10 $ g =: down
  while 12 $ g =: left
  while 18 $ g =: down
  while 12 $ g =: left
  while 7 $ g =: down
  while 8 $ g =: left
  while 10 $ g =: left
  while 10 $ g =: up
  while 14 $ g =: up
  while 20 $ g =: up
  while 15 $ g =: left
  while 8 $ g =: right
  while 15 $ g =: right

welcomeFightMovie :: Scene ()
welcomeFightMovie = do
  w0 <- newActor
  w01 <- newActor
  w02 <- newActor
  let allw0right = allRight [w0, w01, w02]
  w1 <- newActor
  w10 <- newActor
  t0 <- newActor
  mapM_
    (while 10)
    [ w0 =: at' (creatureSprite $ CreatureID General Human) ToRight 0 15
        <> w1 =: at (creatureSprite $ CreatureID Vampire Undead) (lobbiesCellWidth - 1) 11,
      w0 =: right <> w1 =: left,
      w0 =: right <> w0 =: tell "Come on guys!" <> w1 =: left,
      w0 =: right <> w0 =: shutup <> w01 =: at' (creatureSprite $ CreatureID Spearman Human) ToRight 0 15 <> w1 =: tell "Fresh meat!",
      w0 =: right <> w01 =: right <> w02 =: at' (creatureSprite $ CreatureID Archer Human) ToRight 0 15,
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
  while 5 (w1 =: tell "iugp9b7")
  while 1 (w1 =: shutup)
  mapM_ (while 2) (whiteAppears t0 12 11)
  while 10 (w10 =: at (creatureSprite $ CreatureID Skeleton Undead) 12 11)

welcomeMovie :: Scene ()
welcomeMovie = welcomeGhostMovie1 ||| welcomeGhostMovie2 ||| welcomeFightMovie
