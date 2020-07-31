{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Network.WebSockets as WS
import ServerMessages

type Client = (Text, WS.Connection)

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

userNameExists :: UserName -> ServerState -> Bool
userNameExists userName = any ((== userName) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: UserName -> ServerState -> ServerState
removeClient userName = filter ((/= userName) . fst)

broadcast :: WS.WebSocketsData a => a -> ServerState -> IO ()
broadcast message clients = do
  C.putStrLn ("BROADCAST " <> WS.toLazyByteString message)
  forM_ clients $ \(_, conn) ->
    WS.sendTextData conn message

main :: IO ()
main = do
  state <- newMVar newServerState
  WS.runServer "127.0.0.1" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 10
  msg <- WS.receiveData conn
  C.putStrLn ("RECV " <> msg)
  clients <- readMVar state
  case decode msg of
    Just (CreateUser userName)
      | userNameExists userName clients ->
        WS.sendTextData conn (encode (UserCreationFailed UsernameAlreadyExists))
      | otherwise -> flip finally (disconnect userName) $ do
        newState <- modifyMVar_ state $ \clients ->
          return $ addClient (userName, conn) clients
        WS.sendTextData conn (encode UserCreated)
        broadcastUserList state
        talk userName conn state
    Nothing ->
      WS.sendTextData conn (encode ParseError)
  where
    disconnect userName = do
      T.putStrLn ("DISCONNECT " <> userName)
      modifyMVar_ state $ \s ->
        return (removeClient userName s)
      broadcastUserList state

broadcastUserList :: MVar ServerState -> IO ()
broadcastUserList state = do
  clients <- readMVar state
  broadcast (encode (NewUserList (map fst clients))) clients

talk :: UserName -> WS.Connection -> MVar ServerState -> IO ()
talk userName conn state = forever $ do
  msg <- WS.receiveData conn
  C.putStrLn ("RECV(" <> LT.encodeUtf8 (LT.fromStrict userName) <> ") " <> msg)
