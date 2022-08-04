#!/usr/bin/env stack
-- stack --resolver lts-14.11 script
--   --package gloss
--   --package vector
--
-- Search packages by crawling https://www.stackage.org/lts-14.11
--
-- Be sure to install required system dependencies:
-- https://github.com/haskell-opengl/OpenGLRaw/issues/34#issuecomment-598427810
--
-- This script is used to automatically display the composition of the three
-- assets that make the game's main view (see 'GameView'). It is useful
-- when creating these assets with tiled, to have a quick feedback loop.
--
-- Expects four arguments:
--
-- 1. The directory where the next three filenames live
-- 2. The name of the board asset
-- 3. The name of the turn asset
-- 4. The name of the hand asset
--
-- Example call (from the git root):
--
-- ./scripts/DisplayBoard.hs app/assets forest.png turn.png forest-hand.png
-- ./scripts/DisplayBoard.hs app/assets moss-dungeon-board.png moss-dungeon-turn.png moss-dungeon-hand.png

import Codec.Picture
import Data.IORef
import Data.Vector.Storable.ByteString
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Display
import System.Directory
import System.Environment
import System.FSNotify
import System.FilePath

(forestW, forestH) = (504, 624)

(turnW, turnH) = (96, 144)

(handW, handH) = (504, 192)

loadPng :: FilePath -> IO Picture
loadPng path = do
  errorOrImg <- readImage path
  let (Image width height imgData) = either (error . show) convertRGBA8 errorOrImg
  return $
    translate (fromIntegral width / 2) (fromIntegral height / 2) $
      bitmapOfByteString
        width
        height
        (BitmapFormat TopToBottom PxRGBA)
        (vectorToByteString imgData)
        True

assemble :: Picture -> Picture -> Picture -> Picture
assemble forest turn hand =
  translate
    (-forestW / 2)
    (-(forestH + handH) / 2)
    ( translate 0 handH forest
        <> translate (forestW - turnW) handH turn
        <> hand
    )

append dir f = canonicalizePath (dir </> f)

loadImages forestPath turnPath handPath = do
  forestPic <- loadPng forestPath
  turnPic <- loadPng turnPath
  handPic <- loadPng handPath
  return (forestPic, turnPic, handPic)

main = do
  [dir, forest, turn, hand] <- getArgs
  paths@[forestPath, turnPath, handPath] <- traverse (append dir) [forest, turn, hand]
  picsRef <- newIORef =<< loadImages forestPath turnPath handPath
  withManager $ \mgr -> do
    displayIO
      (InWindow "miso-board" (round forestW, round (forestH + handH)) (0, 0))
      black
      ( do
          (forestPic, turnPic, handPic) <- readIORef picsRef
          return (assemble forestPic turnPic handPic)
      )
      ( \(Controller redraw _) -> do
          watchTree
            mgr
            dir
            (\event -> (eventPath event) `elem` paths)
            ( const $ do
                pics <- loadImages forestPath turnPath handPath
                writeIORef picsRef pics
                redraw
            )
          return ()
      )
