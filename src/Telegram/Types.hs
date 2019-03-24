{-# LANGUAGE DeriveGeneric #-}

module Telegram.Types where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text
import           GHC.Generics

data Update = Update
  { updateId :: !Int
  , message  :: !(Maybe Message)
  } deriving (Generic, Show)

instance FromJSON Update where
  parseJSON = genericParseJSON $ aesonDrop 0 snakeCase

data Message = Message
  { messageId :: !Int
  , date      :: !Integer
  , text      :: !Text
  , chat      :: !Chat
  , user      :: !(Maybe User)
  } deriving (Generic, Show)

instance FromJSON Message where
  parseJSON = genericParseJSON $ aesonDrop 0 snakeCase

data Chat = Chat
  { chatId   :: !Int
  , chatType :: !Text
  } deriving (Generic, Show)

instance FromJSON Chat where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data User = User
  { userId    :: !Int
  , firstName :: !Text
  , lastName  :: !(Maybe Text)
  , userName  :: !(Maybe Text)
  } deriving (Generic, Show)

instance FromJSON User where
  parseJSON = genericParseJSON $ aesonDrop 0 snakeCase

data Response = Response
  { ok          :: !Bool
  , result      :: ![Update]
  , description :: !(Maybe String)
  } deriving (Generic, Show)

instance FromJSON Response where
  parseJSON = genericParseJSON $ aesonDrop 0 snakeCase
