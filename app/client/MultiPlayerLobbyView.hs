{-# LANGUAGE OverloadedStrings #-}

module MultiPlayerLobbyView (viewMultiPlayerLobbyModel) where

import Miso
import Miso.String
import Model
import Update

viewMultiPlayerLobbyModel :: MultiPlayerLobbyModel -> View Action
viewMultiPlayerLobbyModel = fmap MultiPlayerLobbyAction' . viewLobby

viewLobby :: MultiPlayerLobbyModel -> View MultiPlayerLobbyAction
viewLobby (CollectingUserName userName) =
  div_
    []
    [ text "user name",
      input_ [onInput $ LobbyUpdateUsername],
      button_ [onClick $ LobbySubmitUsername] [text "submit"]
    ]
viewLobby (WaitingForNameSubmission userName) = text ("submitting " <> ms userName)
viewLobby (DisplayingUserList _ users) =
  ul_
    []
    [li_ [] [text (ms user)] | user <- users]
