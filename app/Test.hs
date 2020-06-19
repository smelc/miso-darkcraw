module Main where

import Card
import System.Exit (exitFailure, exitSuccess)

main :: IO Int
main = do
    putStrLn "Test executes"
    if True then exitSuccess else exitFailure