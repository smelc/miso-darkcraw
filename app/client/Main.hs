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
import qualified Configuration
import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Debug.Trace (traceShow)
import Debugging
import Json (LoadedJson, loadJson)
import Miso
import Model
import qualified Shared
import System.Exit
import System.IO (hPutStrLn, stderr)
import System.Random (getStdGen)
import Update (Action (..), updateModel)
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
    Right loadedJson -> return loadedJson

logTeam :: [Card 'UI] -> Team -> IO ()
logTeam cards t = do
  let deck = mapMaybe cardToCreature $ teamDeck cards t
  print t
  print $ "nb cards: " ++ show (Prelude.length deck)
  print $ "attack: " ++ show (mconcat (Prelude.map attack deck))
  print $ "hp: " ++ show (sum (Prelude.map hp deck))

-- | Entry point for a miso application
main :: IO ()
main = do
  (cards, skills, tiles) <- loadJson'
  stdGen <- getStdGen
  forM_ allTeams (logTeam cards)
  let shared = Shared.create cards skills tiles stdGen
  let model = Model.World' $ Model.World shared -- WelcomeModel' $ initialWelcomeModel shared -- initial model
  runApp $
    if Configuration.isDev
      then startApp $ debugApp NoOp App {..}
      else startApp App {..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    updateLog a m =
      let res = updateModel a m
       in case a of
            NoOp -> res
            _ -> traceShow a res
    updateProd a m = updateModel a m
    update = if Configuration.isDev then updateLog else updateProd
    view = viewModel -- view function
    events = Map.fromList [("mouseleave", True)] <> defaultEvents -- delegated events
    subs = [keyboardSub Keyboard]
    mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')
    logLevel = Off
