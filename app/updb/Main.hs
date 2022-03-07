{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This main executes the balance, specifying to update the
-- regression data; so that the next test of the balance is guaranteed to pass.
--
-- Build with @cabal build updb@, run with @cabal run updb@ (in a @nix-shell@)
module Main where

import qualified Balance
import qualified Shared
import Test.Hspec

main :: IO ()
main = do
  _ <- hspec $ Balance.main (Shared.unsafeGet) True
  return ()
