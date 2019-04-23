{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram.Types where

import           Bot
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.ByteString.Lazy.Char8 as BS
import           Data.Text
import           Data.Text.Encoding
import           GHC.Generics
import           Model
import           Network.HTTP.Simple        (JSONException, Proxy)

type TGRequest r = Request TGModel r

type TGModel = Model TGData

data TGData = TGData
  { offset :: !Int
  } deriving (Show)

data TGResponse result = TGResponse
  { responseOk          :: !Bool
  , responseResult      :: !(Maybe result)
  , responseDescription :: !(Maybe String)
  } deriving (Generic, Show)

instance FromJSON r => FromJSON (TGResponse r) where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data GetUpdate = GetUpdate
  { getUpdateId            :: !Int
  , getUpdateMessage       :: !(Maybe Message)
  , getUpdateCallbackQuery :: !(Maybe CallbackQuery)
  } deriving (Generic, Show)

instance FromJSON GetUpdate where
  parseJSON (Object u) =
    GetUpdate <$> u .: "update_id" <*> u .:? "message" <*>
    u .:? "callback_query"

data SendMessage = SendMessage
  { sendMessageMessage :: !(Maybe Message)
  } deriving (Generic, Show)

instance FromJSON SendMessage where
  parseJSON = genericParseJSON $ aesonDrop 11 snakeCase

data CallbackQuery = CallbackQuery
  { callbackQueryId      :: !Text
  , callbackQueryFrom    :: !User
  , callbackQueryMessage :: !(Maybe Message)
  , callbackQueryData    :: !Text
  } deriving (Generic, Show)

instance FromJSON CallbackQuery where
  parseJSON = genericParseJSON $ aesonDrop 13 snakeCase

data Message = Message
  { messageDate :: !Integer
  , messageText :: !Text
  , messageChat :: !Chat
  , messageFrom :: !User
  } deriving (Generic, Show)

instance FromJSON Message where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data MessageToSend = MessageToSend
  { messageToSendChatId      :: !Int
  , messageToSendText        :: !Text
  , messageToSendReplyMarkup :: !(Maybe InlineKeyboardMarkup)
  } deriving (Generic)

instance ToJSON MessageToSend where
  toJSON (MessageToSend id text Nothing) =
    object ["text" .= text, "chat_id" .= id]
  toJSON (MessageToSend id text (Just kb)) =
    object ["text" .= text, "chat_id" .= id, "reply_markup" .= kb]

instance Show MessageToSend where
  show msg = BS.unpack $ encode $ toJSON msg

data Chat = Chat
  { chatId   :: !Int
  , chatType :: !Text
  } deriving (Generic, Show)

instance FromJSON Chat where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data User = User
  { userId        :: !Int
  , userFirstName :: !Text
  , userLastName  :: !(Maybe Text)
  , userUserName  :: !(Maybe Text)
  } deriving (Generic, Show)

instance FromJSON User where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

newtype InlineKeyboardMarkup = InlineKeyboardMarkup
  { inlineKeyboard :: [[InlineKeyboardButton]]
  } deriving (Generic)

instance ToJSON InlineKeyboardMarkup where
  toJSON = genericToJSON $ aesonDrop 0 snakeCase

data InlineKeyboardButton = InlineKeyboardButton
  { inlineKeyboardButtonText         :: !Text
  , inlineKeyboardButtonCallbackData :: !Text
  } deriving (Generic, Show)

instance ToJSON InlineKeyboardButton where
  toJSON = genericToJSON $ aesonDrop 20 snakeCase
