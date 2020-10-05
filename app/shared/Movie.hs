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

animation :: [Tile] -> Element -> Int -> Int -> [FrameDiff ()]
animation frames e x y =
  [e += at (tileSprite $ head frames) x y]
    <> map (dress e . tileSprite) (tail frames)
    <> [leave e]

blackAppears :: Element -> Int -> Int -> [FrameDiff ()]
blackAppears =
  animation [BlackAppears0, BlackAppears1, BlackAppears2, BlackAppears3]

whiteAppears :: Element -> Int -> Int -> [FrameDiff ()]
whiteAppears =
  animation [WhiteAppears0, WhiteAppears1, WhiteAppears2, WhiteAppears3, WhiteAppears4]

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
  during 12 $ do right g; down g
  during 10 $ do right g; up g
  during 13 $ do right g; down g
  during 10 $ do right g; up g
  during 8 $ right g
  during 17 $ do right g; up g

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
  during 20 $ do right g; down g
  during 15 $ do right g; down g
  during 18 $ do right g; down g
  fork welcomeShade
  during 15 $ do turnAround g; left g

welcomeShade :: Scene ()
welcomeShade = do
  a <- newActor
  s <- newActor
  let (startX, startY) = (lobbiesCellWidth - 1, 2)
  mapM_ (during 2) $ blackAppears a startX startY
  during 6 $ s += at (creatureSprite $ CreatureID Shade Undead) startX startY
  during 5 $ left s

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
    [ do w0 += at' humanGeneral ToRight 0 15; w1 += at undeadVampire (lobbiesCellWidth - 1) 11,
      do right w0; left w1,
      do right w0; tell w0 "Come on guys!"; left w1,
      do shutup w0; w01 += at' humanSpearman ToRight 0 15; tell w1 "Fresh meat!",
      do right w01; w02 += at' humanArcher ToRight 0 15,
      shutup w1,
      do right w0; right w01; up w01; right w02,
      allw0right,
      allw0right,
      do allw0right; left w1,
      do allw0right; left w1,
      do allw0right; left w1,
      do up w0; right w0; right w01; right w02,
      allw0right,
      do up w0; up w01; right w02; up w02
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
  mapM_
    (during 10)
    [ do allw0right; allw1left,
      do up w01; left w1; left w11
    ]
  bonesw12 <- newActor
  during 5 $ do leave w12; bonesw12 += at (tileSprite Bones2) (fst appearsDown - 1) (snd appearsDown); tell w1 "iugp8b4"
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
  blood02 <- newActor
  bonesw11 <- newActor
  blood01 <- newActor
  mapM_
    (during 10)
    [ do leave w02; blood02 += at (tileSprite Blood2) 10 14,
      do leave w11; bonesw11 += at (tileSprite Bones1) 10 11,
      do leave w01; blood01 += at (tileSprite Blood2) 10 12
    ]
  blood0 <- newActor
  sword0 <- newActor
  during 5 $ do leave w0; blood0 += at (tileSprite Blood3) 11 13; sword0 += at (tileSprite Sword2) 11 13
  mapM_
    (during 10)
    [ do left w1; down w1, -- vampire
      tell w1 "Interesting!",
      do tell w1 "Perfect for my collection"; down w10; left w10, -- vampire and archer
      do down w1; left w1; shutup w1; leave sword0; left w20; left w21; down w21; left w22; down w22,
      do left w20; down w20; left w21; down w21; left w22,
      do down w1; left w1; down w10; down w20; left w20; left w21; left w22
    ]
  where
    humanGeneral = creatureSprite $ CreatureID General Human
    humanArcher = creatureSprite $ CreatureID Archer Human
    humanSpearman = creatureSprite $ CreatureID Spearman Human
    undeadArcher = creatureSprite $ CreatureID Archer Undead
    undeadWarrior = creatureSprite $ CreatureID Warrior Undead
    undeadVampire = creatureSprite $ CreatureID Vampire Undead

welcomeMovie :: Scene ()
welcomeMovie = welcomeGhostMovie1 ||| welcomeGhostMovie2 ||| welcomeFightMovie
