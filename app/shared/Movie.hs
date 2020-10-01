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

allLeft :: [Element] -> FrameDiff ()
allLeft actors = mapM_ left actors

allRight :: [Element] -> FrameDiff ()
allRight actors = mapM_ right actors

whiteAppears :: Element -> Int -> Int -> [FrameDiff ()]
whiteAppears e x y =
  [e += at (tileSprite WhiteAppears0) x y]
    <> map (dress e . tileSprite) [WhiteAppears1, WhiteAppears2, WhiteAppears3, WhiteAppears4]
    <> [leave e]

welcomeGhostMovie1 :: Scene ()
welcomeGhostMovie1 = do
  g <- newActor
  during 9 $ g += at' (creatureSprite $ CreatureID Ghost Undead) ToRight 1 0
  during 8 $ down g
  during 15 $ right g
  during 8 $ down g
  during 12 $ right g
  during 7 $ down g
  during 8 $ right g
  during 10 $ down g
  during 20 $ down g
  during 15 $ right g
  during 8 $ right g
  during 15 $ right g

welcomeGhostMovie2 :: Scene ()
welcomeGhostMovie2 = do
  g <- newActor
  during 15 $ g += at (creatureSprite $ CreatureID Ghost Undead) (lobbiesCellWidth - 3) 0
  during 10 $ down g
  during 12 $ left g
  during 18 $ down g
  during 12 $ left g
  during 7 $ down g
  during 8 $ left g
  during 10 $ left g
  during 10 $ up g
  during 14 $ up g
  during 20 $ up g
  during 15 $ left g
  during 8 $ do right g; turnAround g
  during 15 $ right g

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
    (during 10)
    [ do
        w0 += at' humanGeneral ToRight 0 15
        w1 += at undeadVampire (lobbiesCellWidth - 1) 11,
      do
        right w0
        left w1,
      do
        right w0
        tell w0 "Come on guys!"
        left w1,
      do
        right w0
        shutup w0
        w01 += at' humanSpearman ToRight 0 15
        tell w1 "Fresh meat!",
      do
        right w0
        right w01
        w02 += at' humanArcher ToRight 0 15,
      shutup w1,
      do
        right w0
        right w01
        up w01
        right w02,
      allw0right,
      allw0right,
      allw0right,
      do
        allw0right
        left w1,
      do
        allw0right
        left w1,
      do
        allw0right
        left w1,
      do
        up w0
        right w01
        right w02,
      do
        up w0
        right w01
        up w01
        right w02
        up w02
    ]
  during 5 $ tell w1 "iugp9b7"
  during 1 $ shutup w1
  let (appearsUp, appears, appearsDown) = ((12, 10), (12, 11), (12, 12))
  let appearsUpScene = mapM_ (during 2) (uncurry (whiteAppears t0) appearsUp)
  let appearsScene = mapM_ (during 2) (uncurry (whiteAppears t1) appears)
  let appearsDownScene = mapM_ (during 2) (uncurry (whiteAppears t2) appearsDown)
  appearsUpScene ||| appearsScene ||| appearsDownScene
  during 10 $ do
    (\(x, y) -> w10 += at undeadArcher x y) appearsUp
    (\(x, y) -> w11 += at undeadWarrior x y) appears
    (\(x, y) -> w12 += at undeadWarrior x y) appearsDown
  during 1 $ do
    allw0right
    allw1left
  during 5 $ do
    up w01
    left w11
  during 2 $ leave w12
  bonesw12 <- newActor
  during 2 $ bonesw12 += at (tileSprite Bones2) (fst appearsDown - 1) (snd appearsDown)
  during 5 $ tell w1 "iugp8b4"
  during 1 $ shutup w1
  w20 <- newActor
  w21 <- newActor
  w22 <- newActor
  let (newAppearsUp, newAppears, newAppearsDown) = ((11, 12), (10, 13), (11, 14))
  let newAppearsUpScene = mapM_ (during 2) (uncurry (whiteAppears t0) newAppearsUp)
  let newAppearsScene = mapM_ (during 2) (uncurry (whiteAppears t1) newAppears)
  let newAppearsDownScene = mapM_ (during 2) (uncurry (whiteAppears t2) newAppearsDown)
  newAppearsUpScene ||| newAppearsScene ||| newAppearsDownScene
  during 10 $ do
    (\(x, y) -> w20 += at undeadWarrior x y) newAppearsUp
    (\(x, y) -> w21 += at undeadWarrior x y) newAppears
    (\(x, y) -> w22 += at undeadWarrior x y) newAppearsDown
  during 0 $ leave w02
  blood02 <- newActor
  during 1 $ blood02 += at (tileSprite Blood2) 10 14
  during 5 $ leave w11
  bonesw11 <- newActor
  during 1 $ bonesw11 += at (tileSprite Bones1) 10 11
  during 5 $ leave w01
  blood01 <- newActor
  during 1 $ blood01 += at (tileSprite Blood2) 10 12
  during 0 $ leave w0
  blood0 <- newActor
  sword0 <- newActor
  during 10 $ do
    blood0 += at (tileSprite Blood3) 11 13
    sword0 += at (tileSprite Sword2) 11 13
  during 5 $ do
    left w1
    down w1
    left w10
    down w10
  where
    humanGeneral = creatureSprite $ CreatureID General Human
    humanArcher = creatureSprite $ CreatureID Archer Human
    humanSpearman = creatureSprite $ CreatureID Spearman Human
    undeadArcher = creatureSprite $ CreatureID Archer Undead
    undeadWarrior = creatureSprite $ CreatureID Warrior Undead
    undeadVampire = creatureSprite $ CreatureID Vampire Undead

welcomeMovie :: Scene ()
welcomeMovie = welcomeGhostMovie1 ||| welcomeGhostMovie2 ||| welcomeFightMovie
