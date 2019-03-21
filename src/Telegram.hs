{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram where

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.ByteString
import           Data.Text              as T
import           GHC.Generics
import           Logger
import           Model
import qualified Network.HTTP.Client    as Http
import           Network.HTTP.Req
import           Prelude                as P

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
  , chat      :: !Chat
  } deriving (Generic, Show)

instance FromJSON Message where
  parseJSON = genericParseJSON $ aesonDrop 0 $ snakeCase

data Chat = Chat
  { chatId   :: !Int
  , chatType :: !Text
  } deriving (Generic, Show)

instance FromJSON Chat where
  parseJSON = genericParseJSON $ aesonPrefix $ snakeCase

data User = User
  { userId    :: !Int
  , firstName :: !Text
  , lastName  :: !(Maybe Text)
  , userName  :: !(Maybe Text)
  } deriving (Generic, Show)

instance FromJSON User where
  parseJSON = genericParseJSON $ aesonDrop 0 $ snakeCase

data Response = Response
  { ok          :: !Bool
  , result      :: ![Update]
  , description :: !(Maybe String)
  } deriving (Generic, Show)

instance FromJSON Response where
  parseJSON = genericParseJSON $ aesonDrop 0 $ snakeCase

run :: Model -> IO ()
run model = do
  let logWriter = writeLog $ logger model
   in do logWriter (Debug, "Sending a request.")
         response <- getUpdates model
         mapM logWriter $ createLogFromResponse response
         answerForMessages model response
         run $ updateOffset response model

getUpdates :: Model -> IO (Either String Response)
getUpdates model = runReq cfg getReq
  where
    cfg = httpConfig model
    updOfst = offset model
    tkn = token model
    url = (https "api.telegram.org" /: tkn /: "getUpdates")
    getReq = do
      resp <- req GET url NoReqBody lbsResponse $ "offset" =: updOfst
      return $ eitherDecode $ responseBody resp

createLogFromResponse :: Either String Response -> [(LogLevel, String)]
createLogFromResponse (Left errorMessage) =
  [(Errors, "Response is not received. " ++ errorMessage)]
createLogFromResponse (Right (Response False _ (Just desc))) =
  [(Errors, "Server responded with error. " ++ desc)]
createLogFromResponse (Right (Response True updates _)) = [okLog, msgLog]
  where
    okLog = (Debug, "Server responded.")
    msgLog = (Debug, "Got " ++ (show $ P.length $ updates) ++ " messages: " ++ (show $ P.map message updates) ++ ".")

updateOffset :: Either String Response -> Model -> Model
updateOffset (Right (Response True updates@(u:us) _)) model =
  (\m -> m {offset = (updateId . P.last $ updates) + 1}) model
updateOffset _ model = model

answerForMessages :: Model -> Either String Response -> IO ()
answerForMessages model (Right (Response True updates@(u:us) _)) = runReq cfg postReq where
  cfg = httpConfig model
  tkn = token model
  url = https "api.telegram.org" /: tkn /: "sendMessage"
  buildAnswer (Update _ (Just msg)) = "chat_id" =: (chatId . chat $ msg) <> "text" =: (text $ msg) 
  postReq = do
    result <- mapM (\answer -> req POST url NoReqBody ignoreResponse answer) $ P.map buildAnswer updates
    return ()
answerForMessages _ _ = return ()