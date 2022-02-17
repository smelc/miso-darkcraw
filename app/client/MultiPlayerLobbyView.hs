{-# LANGUAGE OverloadedStrings #-}

module MultiPlayerLobbyView (viewMultiPlayerLobbyModel) where

import Data.List
import Data.Maybe (maybeToList)
import Miso
import Miso.String (ToMisoString, ms)
import Model
import Update

viewMultiPlayerLobbyModel :: MultiPlayerLobbyModel -> View Action
viewMultiPlayerLobbyModel = fmap MultiPlayerLobbyAction' . viewLobby

viewLobby :: MultiPlayerLobbyModel -> View MultiPlayerLobbyAction
viewLobby (CollectingUserName _) =
  div_
    []
    [ text "user name",
      input_ [onInput LobbyUpdateUsername],
      button_ [onClick LobbySubmitUsername] [text "submit"]
    ]
viewLobby (WaitingForNameSubmission userName) =
  whoAmIFrame userName $
    text ("submitting " <> ms userName)
viewLobby (DisplayingUserList merror me users)
  | null otherUsers =
      whoAmIFrame me $
        text "there is no other player at the moment"
  | otherwise =
      whoAmIFrame me $
        div_
          []
          ( maybeToList (fmap viewError merror)
              ++ [ text "select user to invite",
                   ul_
                     []
                     [ li_
                         []
                         [button_ [onClick (LobbyInviteUser user)] [text (ms user)]]
                       | user <- otherUsers
                     ]
                 ]
          )
  where
    otherUsers = delete me users
viewLobby (InvitingUser me _ user WaitingForUserInvitationAck) =
  whoAmIFrame me $
    text ("sending invitation to " <> ms user)
viewLobby (InvitingUser me _ user WaitingForRSVP) =
  whoAmIFrame me $
    div_
      []
      [ text ("waiting for user " <> ms user <> " to accept the invitation"),
        button_ [onClick CancelInvitationClicked] [text "Cancel invitation"]
      ]
viewLobby (InvitingUser me _ user WaitingForInvitationDropAck) =
  whoAmIFrame me $
    text ("cancelling invitation to " <> ms user)
viewLobby (Invited me _ user CollectingUserRSVP) =
  whoAmIFrame me $
    div_
      []
      [ text ("you are invited to a game by " <> ms user),
        button_ [onClick AcceptInvitationClicked] [text "Accept"],
        button_ [onClick RejectInvitationClicked] [text "Reject"]
      ]
viewLobby (Invited me _ user WaitingForRejectionAck) =
  whoAmIFrame me $
    text ("rejecting invitation from " <> ms user)
viewLobby (Invited me _ user WaitingForAcceptanceAck) =
  whoAmIFrame me $
    text ("accepting invitation from " <> ms user)
viewLobby (GameStarted me user) =
  whoAmIFrame me $
    text (ms me <> " vs " <> ms user <> ": fight!")

viewError :: MultiPlayerLobbyError -> View a
viewError err = div_ [style_ ("color" =: "red")] [viewError' err]
  where
    viewError' (InvitationRejectedError user) =
      text ("user " <> ms user <> " rejected your invitation")
    viewError' (InvitationCancelledError user) =
      text ("user " <> ms user <> " cancelled their invitation")
    viewError' (UserBusyError user) =
      text ("user " <> ms user <> " is busy")

whoAmIFrame :: ToMisoString str => str -> View action -> View action
whoAmIFrame userName view =
  div_
    []
    [ h3_ [] [text ("Playing as " <> ms userName)],
      view
    ]
