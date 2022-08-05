#!/usr/bin/env stack
-- stack --resolver lts-14.27 runghc

--
-- One can execute ./Main.hs thanks to the shebang line
-- at the top of the file and the stack line below
--
-- To launch [ghcid](https://github.com/ndmitchell/ghcid)
-- in a terminal, do:
--   stack exec ghcid -- --command="ghci Main.hs"
--
-- To disable coc in vim if not working (do it right after opening!):
--   :CocDisable
-- To disable ALE:
--   :ALEDisable
--
-- Both CoC and ALE should work, because they use the *.{yaml,cabal}
-- files in this directory to discover the configuration. That is why
-- we use 'runghc' in the stack configuration above; so that it relies
-- on those files too, as opposed to using 'script' that would ignore
-- those files, see https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.ByteString
import Data.FileEmbed

jsonData :: Data.ByteString.ByteString
jsonData = $(embedFile "data.json")

main = putStr "Hello from Main.hs"
