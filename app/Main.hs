{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Haskell module declaration
module Main where

import Board (exampleBoard)
import Card
import Data.ByteString.Lazy
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Json
import JsonData
import Model
import System.Exit
import System.IO (hPutStrLn, stderr)
import Turn (initialTurn)
import Update
import View

-- Miso framework import
import           Miso
import           Miso.String

-- JSAddle import
#ifndef __GHCJS__
import           Language.Javascript.JSaddle.Warp as JSaddle
import           Network.Wai.Application.Static
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Network.WebSockets
#endif
import           Control.Monad.IO.Class

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

loadJson :: IO [Card UI]
loadJson =
  case parseJson bs of
    Left errMsg -> do
      hPutStrLn  stderr errMsg
      exitWith $ ExitFailure 1
    Right cards -> return cards
  where
    bs :: Data.ByteString.Lazy.ByteString
    bs = Data.ByteString.Lazy.fromStrict jsonData


-- | Entry point for a miso application
main :: IO ()
main = do
  cards :: [Card UI] <- loadJson
  let board = exampleBoard cards
  let model = Model board interaction initialTurn cards -- initial model
  runApp $ startApp App {..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
#ifndef __GHCJS__
    update = logUpdates updateModel -- log events in dev mode
#else
    update = updateModel
#endif
    view   = viewModel            -- view function
    events = Map.fromList [("mouseleave", True), ("mouseenter", True)] <> defaultEvents -- delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')
    interaction = NoInteraction   -- initial interaction