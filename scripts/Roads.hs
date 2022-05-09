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

import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Text as T
import Data.Text.IO
import System.Environment
import System.Process

data Content = Empty | Road
  deriving (Show)

-- >>> dropLast [0, 1]
-- [0]
unsafeDropLast :: [a] -> [a]
unsafeDropLast l = reverse l & tail & reverse

-- >>> prepare "<data encoding='csv>\nfoo1\nfoo2\n</data>"
-- ["foo1", "foo2"]
prepare :: T.Text -> [T.Text]
prepare s = T.lines s & map (T.drop 1) & unsafeDropLast

-- >>> parseLine "0,0,1,2,0,"
-- [Empty, Empty, Road, Road, Empty]
parseLine :: T.Text -> [Content]
parseLine s = T.splitOn "," s & map (\case "0" -> Empty; _ -> Road)

main :: IO ()
main = do
  [world, dest] <- getArgs
  csv <-
    readProcess "xmllint" ["--xpath", "/map/layer[@name='roads and alike']/data", world] ""
      <&> T.pack
  let content = map parseLine $ prepare csv
  print $ T.unpack csv
