{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module containing concrete lists of scenes for lobbies.
-- A movie is a list of scenes, built from the smart operators
-- from 'Cinema'.
module Movie (welcomeMovie) where

import Card (CreatureID (..), CreatureKind (..), Team (..))
import Cinema
import Constants
import Data.Function ((&))
import Tile

allLeft :: [Element] -> Scene ()
allLeft actors = mapM_ left actors

allRight :: [Element] -> Scene ()
allRight actors = mapM_ right actors

animation :: [Tile] -> Element -> Int -> Int -> [Scene ()]
animation frames e x y =
  [do e & moveTo x y; e & dress (tileSprite $ head frames)]
    <> map (flip dress e . tileSprite) (tail frames)
    <> [hide e]

blackAppears :: Element -> Int -> Int -> [Scene ()]
blackAppears =
  animation [BlackAppears0, BlackAppears1, BlackAppears2, BlackAppears3]

blackDisappears :: Element -> Int -> Int -> [Scene ()]
blackDisappears =
  animation $ reverse [BlackAppears0, BlackAppears1, BlackAppears2, BlackAppears3]

whiteAppears :: Element -> Int -> Int -> [Scene ()]
whiteAppears =
  animation [WhiteAppears0, WhiteAppears1, WhiteAppears2, WhiteAppears3, WhiteAppears4]

welcomeGhostMovie1 :: Scene ()
welcomeGhostMovie1 = do
  g <- during 9 $ newActorAt' "g1" (creatureSprite $ CreatureID Ghost Undead) ToRight 1 0
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
  during 12 $ do turnAround g; left g
  during 14 $ do left g; up g
  during 18 $ do left g; up g
  during 14 $ do left g; up g
  during 18 $ do left g; up g
  hide g

welcomeGhostMovie2 :: Scene ()
welcomeGhostMovie2 = do
  g <- during 15 $ newActorAt "g2" (creatureSprite $ CreatureID Ghost Undead) (lobbiesCellWidth - 3) 0
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
  during 12 $ left g
  during 18 $ do left g; up g
  hide g

welcomeShade :: Scene ()
welcomeShade = do
  let (startX, startY) = (lobbiesCellWidth - 1, 2)
  a <- newHiddenActor "a"
  mapM_ (during 2) $ blackAppears a startX startY
  s <- during 6 $ newActorAt "s" (creatureSprite $ CreatureID Shade Undead) startX startY
  during 5 $ left s
  during 5 $ do left s; down s
  during 6 $ do left s; up s
  during 6 $ left s
  d <- newHiddenActor "d"
  mapM_ (during 2) $ blackDisappears d 15 2
  during 7 $ left s
  during 6 $ left s
  during 5 $ do left s; up s
  during 6 $ do left s; up s
  during 5 $ do left s; down s
  during 6 $ do left s; down s
  during 8 $ do left s; down s
  during 5 $ do left s; down s
  during 7 $ do left s; down s
  during 6 $ do left s; down s
  during 6 $ left s
  during 5 $ do left s; down s
  during 6 $ do left s; down s
  during 8 $ left s
  during 5 $ do left s; down s
  during 7 $ do left s; down s
  during 6 $ down s
  during 1 $ hide s

welcomeFightMovie :: Scene ()
welcomeFightMovie = do
  w0 <- newActorAt' "w0" humanGeneral ToRight 0 15
  w01 <- newHiddenActor "w01"
  w02 <- newHiddenActor "w02"
  let allw0right = allRight [w0, w01, w02]
  w1 <- newActorAt "w1" undeadVampire (lobbiesCellWidth - 1) 11
  w10 <- newHiddenActor "w10"
  w11 <- newHiddenActor "w11"
  w12 <- newHiddenActor "w12"
  let allw1left = allLeft [w1, w10, w11, w12]
  t0 <- newHiddenActor "t0"
  t1 <- newHiddenActor "t1"
  t2 <- newHiddenActor "t2"
  mapM_
    (during 10)
    [ do right w0; left w1,
      do right w0; w0 & tell "Come on guys!"; left w1,
      do shutup w0; w01 & resetAt' humanSpearman ToRight 0 15; w1 & tell "Fresh meat!",
      do right w01; w02 & resetAt' humanArcher ToRight 0 15,
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
  during 5 $ w1 & tell "iugp9b7"
  during 1 $ shutup w1
  let (appearsUp, appears, appearsDown) = ((12, 10), (12, 11), (12, 12))
  let appearsUpScene = mapM_ (during 2) (uncurry (whiteAppears t0) appearsUp)
  let appearsScene = mapM_ (during 2) (uncurry (whiteAppears t1) appears)
  let appearsDownScene = mapM_ (during 2) (uncurry (whiteAppears t2) appearsDown)
  appearsUpScene ||| appearsScene ||| appearsDownScene
  during 10 $ do
    (\(x, y) -> w10 & resetAt undeadArcher x y) appearsUp
    (\(x, y) -> w11 & resetAt undeadWarrior x y) appears
    (\(x, y) -> w12 & resetAt undeadWarrior x y) appearsDown
  mapM_
    (during 10)
    [ do allw0right; allw1left,
      do up w01; left w1; left w11
    ]
  _ <- newActorAt "bones" (tileSprite Bones2) (fst appearsDown - 1) (snd appearsDown)
  during 5 $ do hide w12; w1 & tell "iugp8b4"
  during 1 $ shutup w1
  w20 <- newHiddenActor "w20"
  w21 <- newHiddenActor "w21"
  w22 <- newHiddenActor "w22"
  let (newAppearsUp, newAppears, newAppearsDown) = ((11, 12), (10, 13), (11, 14))
  let newAppearsUpScene = mapM_ (during 2) (uncurry (whiteAppears t0) newAppearsUp)
  let newAppearsScene = mapM_ (during 2) (uncurry (whiteAppears t1) newAppears)
  let newAppearsDownScene = mapM_ (during 2) (uncurry (whiteAppears t2) newAppearsDown)
  newAppearsUpScene ||| newAppearsScene ||| newAppearsDownScene
  during 10 $ do
    (\(x, y) -> w20 & resetAt undeadWarrior x y) newAppearsUp
    (\(x, y) -> w21 & resetAt undeadWarrior x y) newAppears
    (\(x, y) -> w22 & resetAt undeadWarrior x y) newAppearsDown
  blood02 <- newHiddenActor "blood02"
  bonesw11 <- newHiddenActor "bonesw11"
  blood01 <- newHiddenActor "blood01"
  mapM_
    (during 10)
    [ do hide w02; blood02 & resetAt (tileSprite Blood2) 10 14,
      do hide w11; bonesw11 & resetAt (tileSprite Bones1) 10 11,
      do hide w01; blood01 & resetAt (tileSprite Blood2) 10 12
    ]
  blood0 <- newHiddenActor "blood0"
  sword0 <- newHiddenActor "sword0"
  during 5 $ do hide w0; blood0 & resetAt (tileSprite Blood3) 11 13; sword0 & resetAt (tileSprite Sword2) 11 13
  mapM_
    (during 10)
    [ do left w1; down w1, -- vampire
      w1 & tell "Interesting!",
      do w1 & tell "Perfect for my collection"; down w10; left w10, -- vampire and archer
      do down w1; left w1; shutup w1; hide sword0; left w20; left w21; down w21; left w22; down w22,
      do left w20; down w20; left w21; down w21; left w22,
      do down w1; left w1; down w10; down w20; left w20; left w21; left w22,
      do down w1; left w1; down w10; left w20; left w21; left w22,
      do left w1; down w10; left w10; left w20; left w21; left w22,
      do left w1; left w10; left w20; left w21; left w22,
      do left w1; left w10; left w20; left w21; left w22,
      do left w1; left w10; left w20; left w21; left w22,
      do left w1; left w10; left w20; left w21; left w22,
      do left w1; left w10; left w20; left w21; left w22,
      do left w1; left w10; left w20; hide w21; left w22,
      do left w1; hide w20; hide w22,
      do left w1; left w10,
      do hide w1; left w10,
      do left w10; down w10,
      hide w10
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
