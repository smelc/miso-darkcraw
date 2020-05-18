#!/usr/bin/env stack
-- stack --resolver lts-14.11 script

import Codec.Picture
import Codec.Picture.Extra
import Control.Monad
import Control.Lens
import qualified Data.Vector.Storable as V
import System.Directory
import System.FilePath
import Text.Printf

sizes = [16, 24]
inputDir = "assets"
inputFile size = inputDir </> printf "%dx%d.png" size size
outputDir = "app" </> "assets"
outputFile size (x, y) = outputDir </> printf "%dx%d_%d_%d.png" size size x y

openInputFile size = do
  errorOrImg <- readImage (inputFile size)
  return $ either (error . show) convertRGBA8 errorOrImg

genIndices size img =
  [ (i, j)
  | i <- [0 .. (imageWidth img `div` size) - 1]
  , j <- [0 .. (imageHeight img `div` size) - 1]
  ]

isTransparent = allOf imagePixels isTransparentPixel
 where
  isTransparentPixel (PixelRGBA8 _ _ _ 0) = True
  isTransparentPixel _ = False

genImages size = do
  img <- openInputFile size
  let indices = genIndices size img
  let tiles = [crop (i*size) (j*size) size size img | (i,j) <- indices]
  let fileNames = map (outputFile size) indices
  let namedTiles = zip fileNames tiles
  mapM_ (uncurry writePng) (filter (not . isTransparent . snd) namedTiles)

main = do
  createDirectoryIfMissing False outputDir
  mapM_ genImages sizes

