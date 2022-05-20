#!/usr/bin/env stack
-- stack --resolver lts-14.11 script

-- This script is used to automatically generate the list of roads
-- from tiled/world.tmx
--
-- It expects two arguments:
--
-- 1. The tmx file to read
-- 2. The file to write the output to (gets erased if existing)
--
-- Example call (from the git root):
--
-- ./scripts/Roads.hs tiled/world.tmx app/client/Roads.hs
--
-- To develop this file with hls support:
--
-- cd app
-- nix-shell
-- code ../scripts/
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Set as Set
import Data.Text (intersperse)
import qualified Data.Text as T
import Data.Text.IO
import System.Environment
import System.Process

data Content = Empty | Road
  deriving (Show)

-- >>> prepare "<data encoding='csv'>\nfoo1\nfoo2\n</data>"
-- ["foo1", "foo2"]
prepare :: T.Text -> [T.Text]
prepare s = T.lines s & filter f & map (T.drop 1) & init
  where
    f l
      | T.null l = False
      | otherwise = T.take 1 l /= "<"

-- >>> parseLine "0,0,1,2,0,"
-- [Empty, Empty, Road, Road, Empty]
parseLine :: T.Text -> [Content]
parseLine s = T.splitOn "," s & filter (not . T.null) & map (\case "0" -> Empty; _ -> Road)

type Line = [(Int, Int)] -- (x, y)

-- | @toCoords y content@ returns @content@ transformed into (x,y)
-- coordinates. @y@ is the -- y coordinate of @content@
toCoords :: Int -> [Content] -> Line
toCoords y content =
  go (zip [1 ..] content)
  where
    go = \case
      [] -> []
      (_, Empty) : rest -> go rest
      (x, Road) : rest -> (x, y) : go rest

moduleStart :: [T.Text]
moduleStart =
  [ "module Roads where",
    "",
    "import Nat",
    "",
    "points :: [[(Nat, Nat)]]",
    "points ="
  ]

moduleEnd :: [T.Text]
moduleEnd = ["  ]"]

coordToHaskell :: (Int, Int) -> T.Text
coordToHaskell (x, y) = "(" <> T.pack (show x) <> ", " <> T.pack (show y) <> ")"

coordsToHaskell :: Line -> T.Text
coordsToHaskell =
  \case
    [] -> "[]"
    x : rest -> coordToHaskell x <> " : " <> coordsToHaskell rest

-- | @mapButLast f l@ applies @f@ to all elements of @l@ except the last one
mapButLast :: (a -> a) -> [a] -> [a]
mapButLast f =
  \case
    [] -> []
    [x] -> [x]
    x : rest -> f x : mapButLast f rest

-- | @mapButFirstf l@ applies @f@ to all elements of @l@ except the first one
mapButFirst :: (a -> a) -> [a] -> [a]
mapButFirst f =
  \case
    [] -> []
    [x] -> [x]
    x : rest -> x : map f rest

main :: IO ()
main = do
  [world, dest] <- getArgs
  csv <-
    readProcess "xmllint" ["--xpath", "/map/layer[@name='roads and alike']/data", world] ""
      <&> T.pack
  let content :: [[Content]] =
        prepare csv
          & map parseLine
      lines :: [Line] = zipWith toCoords [1 ..] content
      linesText :: [T.Text] =
        map coordsToHaskell lines
          & mapButFirst ("    " <>)
          & mapButLast (<> ",")
  Data.Text.IO.writeFile dest (T.unlines moduleStart <> "  [ " <> T.unlines (linesText ++ moduleEnd))
  Prelude.putStrLn ("Written " ++ dest)
