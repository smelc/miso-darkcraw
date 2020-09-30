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

allLeft :: [Element] -> Frame ActorChange
allLeft actors = mconcat [a =: left | a <- actors]

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
  while 8 $ g =: right <> g =: turnAround
  while 15 $ g =: right

welcomeFightMovie :: Scene ()
welcomeFightMovie = do
  w0 <- newActor
  w01 <- newActor
  w02 <- newActor
  let allw0right = allRight [w0, w01, w02]
  w1 <- newActor
  w10 <- newActor
  w11 <- newActor
  w12 <- newActor
  let allw1left = allLeft [w1, w10, w11, w12]
  t0 <- newActor
  t1 <- newActor
  t2 <- newActor
  mapM_
    (while 10)
    [ w0 =: at' (creatureSprite $ CreatureID General Human) ToRight 0 15
        <> w1 =: at (creatureSprite $ CreatureID Vampire Undead) (lobbiesCellWidth - 1) 11,
      w0 =: right <> w1 =: left,
      w0 =: right <> w0 =: tell "Come on guys!" <> w1 =: left,
      w0 =: right <> w0 =: shutup <> w01 =: at' humanSpearman ToRight 0 15 <> w1 =: tell "Fresh meat!",
      w0 =: right <> w01 =: right <> w02 =: at' humanArcher ToRight 0 15,
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
  let (appearsUp, appears, appearsDown) = ((12, 10), (12, 11), (12, 12))
  let appearsUpScene = mapM_ (while 2) (uncurry (whiteAppears t0) appearsUp)
  let appearsScene = mapM_ (while 2) (uncurry (whiteAppears t1) appears)
  let appearsDownScene = mapM_ (while 2) (uncurry (whiteAppears t2) appearsDown)
  appearsUpScene ||| appearsScene ||| appearsDownScene
  while 10 (w10 =: uncurry (at undeadArcher) appearsUp)
    ||| while 10 (w11 =: uncurry (at undeadWarrior) appears)
    ||| while 10 (w12 =: uncurry (at undeadWarrior) appearsDown)
  while 1 allw0right ||| while 1 allw1left
  while 5 (w01 =: up) ||| while 5 (w11 =: left)
  while 2 (w12 =: Leave)
  bonesw12 <- newActor
  while 2 $ bonesw12 =: at (tileSprite Bones2) (fst appearsDown - 1) (snd appearsDown)
  while 5 (w1 =: tell "iugp8b4")
  while 1 (w1 =: shutup)
  w20 <- newActor
  w21 <- newActor
  w22 <- newActor
  let (newAppearsUp, newAppears, newAppearsDown) = ((11, 12), (10, 13), (11, 14))
  let newAppearsUpScene = mapM_ (while 2) (uncurry (whiteAppears t0) newAppearsUp)
  let newAppearsScene = mapM_ (while 2) (uncurry (whiteAppears t1) newAppears)
  let newAppearsDownScene = mapM_ (while 2) (uncurry (whiteAppears t2) newAppearsDown)
  newAppearsUpScene ||| newAppearsScene ||| newAppearsDownScene
  while 10 (w20 =: uncurry (at undeadWarrior) newAppearsUp)
    ||| while 10 (w21 =: uncurry (at undeadWarrior) newAppears)
    ||| while 10 (w22 =: uncurry (at undeadWarrior) newAppearsDown)
  while 0 (w02 =: Leave)
  blood02 <- newActor
  while 1 (blood02 =: at (tileSprite Blood2) 10 14)
  while 5 (w11 =: Leave)
  bonesw11 <- newActor
  while 1 (bonesw11 =: at (tileSprite Bones1) 10 11)
  while 5 (w01 =: Leave)
  blood01 <- newActor
  while 1 (blood01 =: at (tileSprite Blood2) 10 12)
  while 0 (w0 =: Leave)
  blood0 <- newActor
  sword0 <- newActor
  while 10 (blood0 =: at (tileSprite Blood3) 11 13)
    ||| while 10 (sword0 =: at (tileSprite Sword2) 11 13)
  while 5 (w1 =: left <> w1 =: down <> w10 =: left <> w10 =: down) -- vampire and archer
  -- while 5 (w12 =: left <> w21 =: down <> w21 =: left <> w22 =: left)
  where
    humanArcher = creatureSprite $ CreatureID Archer Human
    humanSpearman = creatureSprite $ CreatureID Spearman Human
    undeadArcher = creatureSprite $ CreatureID Archer Undead
    undeadWarrior = creatureSprite $ CreatureID Warrior Undead

welcomeMovie :: Scene ()
welcomeMovie = welcomeGhostMovie1 ||| welcomeGhostMovie2 ||| welcomeFightMovie
