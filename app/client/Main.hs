{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

#ifndef __GHCJS__
import           Language.Javascript.JSaddle.Warp as JSaddle
import           Network.Wai.Application.Static
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Network.WebSockets
#endif

import Card
import Configuration (hashless)
import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Debugging
import Json (LoadedJson, loadJson)
import Miso
import Model
import SharedModel (SharedModel (..))
import System.Exit
import System.IO (hPutStrLn, stderr)
import System.Random (getStdGen)
import Update (Action (..), initialWelcomeModel, updateModel)
import View (viewModel)

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp f =
  Warp.runSettings (Warp.setPort 8080 (Warp.setTimeout 3600 Warp.defaultSettings)) =<<
    JSaddle.jsaddleOr defaultConnectionOptions (f >> syncPoint) app
  where app req sendResp =
          case Wai.pathInfo req of
            ("assets" : _) -> staticApp (defaultWebAppSettings ".") req sendResp
            _ -> JSaddle.jsaddleApp req sendResp
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

loadJson' :: IO LoadedJson
loadJson' =
  case loadJson of
    Left errMsg -> do
      hPutStrLn stderr errMsg
      exitWith $ ExitFailure 1
    Right cards -> return cards

logTeam :: [Card UI] -> Team -> IO ()
logTeam cards t = do
  let deck = mapMaybe cardToCreature $ teamDeck cards t
  print t
  print $ "nb cards: " ++ show (Prelude.length deck)
  print $ "attack: " ++ show (sum (Prelude.map attack deck))
  print $ "hp: " ++ show (sum (Prelude.map hp deck))

-- | Entry point for a miso application
main :: IO ()
main = do
  (cards, skills, tiles) <- loadJson'
  stdGen <- getStdGen
  forM_ allTeams (logTeam cards)
  let shared =
        SharedModel
          { sharedCards = cards,
            sharedSkills = skills,
            sharedTiles = tiles,
            sharedStdGen = stdGen
          }
  let model = WelcomeModel' $ initialWelcomeModel shared -- initial model
  runApp $
    if hashless
      then startApp $ debugApp NoOp App {..}
      else startApp App {..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    update = updateModel
    view = viewModel -- view function
    events = Map.fromList [("mouseleave", True)] <> defaultEvents -- delegated events
    subs = [keyboardSub Keyboard]
    mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')
    logLevel = Off
