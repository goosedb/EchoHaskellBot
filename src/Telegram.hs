{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Telegram where

import           Data.Text as T
import           Data.Aeson
import           Network.HTTP.Req
import           Data.Aeson.Casing
import           GHC.Generics
import           State
import           Control.Monad.IO.Class

data Update = Update
  { updateId :: !Int
  , message  :: !(Maybe Message)
  } deriving (Generic, Show)

instance FromJSON Update where
  parseJSON = genericParseJSON $ aesonDrop 0 $ snakeCase

data Message = Message
  { messageId :: !Int
  , date      :: !Integer
  , text      :: !Text
  --, chat      :: !Chat
  } deriving (Generic, Show)

instance FromJSON Message where
  parseJSON = genericParseJSON $ aesonDrop 0 $ snakeCase

data Chat = Chat
  { chatId   :: !Int
  , chatType :: !Text
  } deriving (Generic, Show)

instance FromJSON Chat where
  parseJSON = genericParseJSON $ aesonDrop 0 $ snakeCase

data User = User
  { userId    :: !Int
  , firstName :: !Text
  , lastName  :: !(Maybe Text)
  , userName  :: !(Maybe Text)
  } deriving (Generic, Show)

instance FromJSON User where
  parseJSON = genericParseJSON $ aesonDrop 0 $ snakeCase

data Response = Response
  { ok     :: Bool
  , result :: [Update]
  } deriving (Generic, Show)

instance FromJSON Response where
  parseJSON = genericParseJSON $ aesonDrop 0 $ snakeCase


run :: State -> IO ()
run state = do
  response <- getUpdates state
  print response
  run state

getUpdates :: State -> IO (Either String Response)
getUpdates state = runReq cfg getReq where
  cfg = httpConfig state
  reqOffset = offset state
  tkn = T.concat ["bot", token state]
  url = (https "api.telegram.org" /: tkn /: "getUpdates") 
  getReq = do 
    resp <- req GET url NoReqBody lbsResponse $ "offset" =: reqOffset
    liftIO $ return $ eitherDecode $ responseBody resp
    
