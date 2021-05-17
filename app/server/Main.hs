{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Control.Lens hiding (from, to)
import Control.Monad (forM_, forever)
import Control.Monad.State (execState)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Generics.Labels ()
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import GHC.Generics hiding (from, to)
import qualified Network.WebSockets as WS
import ServerMessages

{- Datatypes -}

data ClientState
  = ClientIdle
  | ClientWaitingRsvpFrom UserName
  | ClientConsideringInvitationFrom UserName
  | ClientPlayingWith UserName
  deriving (Eq, Show, Generic)

data Client = Client
  { clientConnection :: WS.Connection,
    clientState :: ClientState
  }
  deriving (Show, Generic)

instance Show WS.Connection where
  show _ = "SomeConnection"

newtype ServerState = ServerState {serverStateClients :: M.Map UserName Client}
  deriving (Show, Generic)

data Recipient = EveryOne | SomeUser UserName
  deriving (Show, Generic)

data MessageToSend = MessageToSend
  { toSendRecipient :: Recipient,
    toSendMessage :: OutMessage
  }
  deriving (Show, Generic)

makeNewServerState :: ServerState
makeNewServerState = ServerState M.empty

{- ServerState queries -}

clientOf :: UserName -> Lens' ServerState (Maybe Client)
clientOf user = #serverStateClients . at user

clientStateOf :: UserName -> Traversal' ServerState ClientState
clientStateOf user = clientOf user . _Just . #clientState

userNameExists :: UserName -> ServerState -> Bool
userNameExists userName ServerState {serverStateClients} =
  userName `M.member` serverStateClients

usersMatching :: (ClientState -> Bool) -> ServerState -> [UserName]
usersMatching predicate state =
  state
    & serverStateClients
    & M.toList
    & filter (predicate . clientState . snd)
    & map fst

idleUsers :: ServerState -> [UserName]
idleUsers = usersMatching (== ClientIdle)

usersWaitingRsvpFrom :: UserName -> ServerState -> [UserName]
usersWaitingRsvpFrom user = usersMatching (== ClientWaitingRsvpFrom user)

usersConsideringInvitationFrom :: UserName -> ServerState -> [UserName]
usersConsideringInvitationFrom user = usersMatching (== ClientConsideringInvitationFrom user)

usersPlayingWith :: UserName -> ServerState -> [UserName]
usersPlayingWith user = usersMatching (== ClientPlayingWith user)

{- ServerState updates -}

addClient :: CanSendMessages m => UserName -> WS.Connection -> ServerState -> m ServerState
addClient user conn state = do
  send user conn UserCreated
  broadcast newState (NewUserList (idleUsers newState))
  return newState
  where
    newState = state & #serverStateClients . at user ?~ Client conn ClientIdle

removeClient :: CanSendMessages m => UserName -> ServerState -> m ServerState
removeClient user state = do
  broadcast newState (NewUserList (idleUsers newState))
  forM_ invitationsToCancel $ \other ->
    sendToName newState other InvitationRejected
  forM_ invitationsToReject $ \other ->
    sendToName newState other IncomingInvitationCancelled
  forM_ gamesToLeave $ \other ->
    sendToName newState other OtherPlayerLeftGame
  return newState
  where
    invitationsToCancel = usersWaitingRsvpFrom user state
    invitationsToReject = usersConsideringInvitationFrom user state
    gamesToLeave = usersPlayingWith user state
    newState = flip execState state $ do
      clientOf user .= Nothing
      forM_ (invitationsToCancel ++ invitationsToReject ++ gamesToLeave) $ \other ->
        clientStateOf other .= ClientIdle

createInvitation :: CanSendMessages m => UserName -> UserName -> ServerState -> m ServerState
createInvitation from to state
  | state ^? clientStateOf from /= Just ClientIdle = do
    sendToName state from UnexpectedMessage
    return state
  | state ^? clientStateOf to /= Just ClientIdle = do
    sendToName state from UserBusy
    return state
  | otherwise = do
    sendToName state to (IncomingInvitation from)
    sendToName state from InvitationSent
    broadcastNewUserList $
      state
        & clientStateOf from .~ ClientWaitingRsvpFrom to
        & clientStateOf to .~ ClientConsideringInvitationFrom from

cancelInvitationFrom :: CanSendMessages m => UserName -> ServerState -> m ServerState
cancelInvitationFrom from state
  | Just (ClientWaitingRsvpFrom to) <- state ^? clientStateOf from = do
    sendToName state to IncomingInvitationCancelled
    sendToName state from InvitationDropAck
    broadcastNewUserList $
      state
        & clientStateOf from .~ ClientIdle
        & clientStateOf to .~ ClientIdle
  | otherwise = do
    sendToName state from UnexpectedMessage
    return state

rejectInvitationTo :: CanSendMessages m => UserName -> ServerState -> m ServerState
rejectInvitationTo to state
  | Just (ClientConsideringInvitationFrom from) <- state ^? clientStateOf to = do
    sendToName state from InvitationRejected
    sendToName state to IncomingInvitationRejectionAck
    broadcastNewUserList $
      state
        & clientStateOf from .~ ClientIdle
        & clientStateOf to .~ ClientIdle
  | otherwise = do
    sendToName state to UnexpectedMessage
    return state

acceptInvitationTo :: CanSendMessages m => UserName -> ServerState -> m ServerState
acceptInvitationTo to state
  | Just (ClientConsideringInvitationFrom from) <- state ^? clientStateOf to = do
    sendToName state from InvitationAccepted
    sendToName state to IncomingInvitationAcceptanceAck
    broadcastNewUserList $
      state
        & clientStateOf from .~ ClientPlayingWith to
        & clientStateOf to .~ ClientPlayingWith from
  | otherwise = do
    sendToName state to UnexpectedMessage
    return state

broadcastNewUserList :: CanSendMessages m => ServerState -> m ServerState
broadcastNewUserList state = do
  broadcast state (NewUserList (idleUsers state))
  return state

{- Communication -}

class Monad m => CanSendMessages m where
  send :: UserName -> WS.Connection -> OutMessage -> m ()

instance CanSendMessages IO where
  send userName conn msg = do
    T.putStrLn ("SEND(" <> userName <> "): " <> T.pack (show msg))
    WS.sendTextData conn (encode msg)

sendToName :: CanSendMessages m => ServerState -> UserName -> OutMessage -> m ()
sendToName state userName msg =
  forM_ (state ^? #serverStateClients . ix userName . #clientConnection) $ \conn ->
    send userName conn msg

broadcast :: CanSendMessages m => ServerState -> OutMessage -> m ()
broadcast state msg =
  forM_ (M.toList (serverStateClients state)) $ \(userName, Client conn _) ->
    send userName conn msg

receive :: UserName -> WS.Connection -> IO InMessage
receive userName conn = do
  bytes <- WS.receiveData conn
  C.putStrLn ("RECV(" <> LT.encodeUtf8 (LT.fromStrict userName) <> "): " <> bytes)
  case decode bytes of
    Just msg -> return msg
    Nothing -> do
      send userName conn ParseError
      receive userName conn

{- Main loop -}

main :: IO ()
main = do
  state <- newMVar makeNewServerState
  WS.runServer "127.0.0.1" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application stateVar pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 10
  msg <- receive "" conn
  clients <- readMVar stateVar
  case msg of
    CreateUser userName
      | userNameExists userName clients ->
        send "" conn (UserCreationFailed UsernameAlreadyExists)
      | otherwise -> flip finally (disconnect userName) $ do
        modifyMVar_ stateVar (addClient userName conn)
        talk userName conn stateVar
    _ ->
      send "" conn UnexpectedMessage
  where
    disconnect userName = do
      T.putStrLn ("DISCONNECT " <> userName)
      modifyMVar_ stateVar (removeClient userName)
      return ()

talk :: UserName -> WS.Connection -> MVar ServerState -> IO ()
talk user conn stateVar = forever $ do
  msg <- receive user conn
  case msg of
    InviteUser other ->
      modifyMVar_ stateVar (createInvitation user other)
    DropInvitation ->
      modifyMVar_ stateVar (cancelInvitationFrom user)
    RejectInvitation ->
      modifyMVar_ stateVar (rejectInvitationTo user)
    AcceptInvitation ->
      modifyMVar_ stateVar (acceptInvitationTo user)
    _ ->
      return ()
