{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Haskell module declaration
module Main where

import Board (exampleBoard)
import Card
-- Miso framework import

-- JSAddle import
#ifndef __GHCJS__
import           Language.Javascript.JSaddle.Warp as JSaddle
import           Network.Wai.Application.Static
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Network.WebSockets
#endif
import Control.Monad.IO.Class
import Control.Monad (forM_)
import Data.ByteString.Lazy (ByteString, fromStrict)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Json (loadJson)
import JsonData
import Miso
import Miso.String
import Model
import System.Exit
import System.IO (hPutStrLn, stderr)
import Turn (initialTurn)
import Update
import View

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

loadJson' :: IO [Card UI]
loadJson' =
  case loadJson of
    Left errMsg -> do
      hPutStrLn stderr errMsg
      exitWith $ ExitFailure 1
    Right cards -> return cards
  where
    bs :: Data.ByteString.Lazy.ByteString
    bs = Data.ByteString.Lazy.fromStrict jsonData

logTeam :: [Card UI] -> Team -> IO ()
logTeam cards t = do
  let deck = Prelude.map cardToCreature $ initialDeck cards t
  print t
  print $ "nb cards: " ++ show (Prelude.length deck)
  print $ "attack: " ++ show (sum (Prelude.map attack deck))
  print $ "hp: " ++ show (sum (Prelude.map hp deck))

-- | Entry point for a miso application
main :: IO ()
main = do
  cards :: [Card UI] <- loadJson'
  forM_ allTeams (logTeam cards)
  let board = exampleBoard cards
  let model = GameModel' $ GameModel board interaction initialTurn cards mempty -- initial model
  runApp $ startApp App {..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
#ifndef __GHCJS__
    update = logUpdates updateModel -- log events in dev mode
#else
    update = updateModel
#endif
    view = viewModel -- view function
    events = Map.fromList [("mouseleave", True)] <> defaultEvents -- delegated events
    subs = [] -- empty subscription list
    mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')
    interaction = NoInteraction -- initial interaction
    logLevel = Off
