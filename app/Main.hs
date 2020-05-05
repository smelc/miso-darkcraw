-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

-- | Haskell module declaration
module Main where

import Model
import Update
import View

-- | Miso framework import
import           Miso
import           Miso.String

-- | JSAddle import
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

-- | Entry point for a miso application
main :: IO ()
main = runApp $ startApp App {..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model  = 0                    -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')
