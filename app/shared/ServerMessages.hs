{-# LANGUAGE DeriveGeneric #-}

module ServerMessages where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

type UserName = Text

data InMessage = CreateUser UserName
  deriving (Eq, Show, Generic)

instance FromJSON InMessage

instance ToJSON InMessage

data UserCreationError = UsernameAlreadyExists
  deriving (Eq, Show, Generic)

instance FromJSON UserCreationError

instance ToJSON UserCreationError

data OutMessage
  = ParseError
  | UserCreated
  | UserCreationFailed UserCreationError
  | NewUserList [UserName]
  deriving (Eq, Show, Generic)

instance FromJSON OutMessage

instance ToJSON OutMessage
