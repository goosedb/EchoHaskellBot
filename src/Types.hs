{-# LANGUAGE DeriveGeneric #-}

module Types where

import           Data.Aeson
import           Data.Text
import           GHC.Generics

data Service
  = Telegram
  | Slack
  deriving (Show, Generic)

instance FromJSON Service

instance ToJSON Service

data DialogState
  = Question
  | Common
  deriving (Show)

data UserState = UserState
  { stateId     :: !Int
  , repNum      :: !Int
  , dialogState :: !DialogState
  } deriving (Show)

instance Eq UserState where
  (UserState id1 _ _) == (UserState id2 _ _) = id1 == id2

type UserStates = [UserState]

data Proxy = Proxy
  { host :: !Text
  , port :: !Int
  } deriving (Show, Generic)

instance FromJSON Proxy

instance ToJSON Proxy
