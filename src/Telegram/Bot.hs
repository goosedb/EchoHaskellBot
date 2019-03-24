{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot where

import           Bot
import qualified Config                   as Cfg
import           Data.Aeson
import qualified Data.ByteString          as BS
import           Data.List
import           Data.Maybe
import qualified Data.Text                as T
import           Logger
import qualified Model                    as Mdl
import qualified Network.HTTP.Client      as Cl
import           Network.HTTP.Req         as Req
import           Telegram.Types
import           Web.Internal.HttpApiData

data Telegram =
  Telegram

type Model = Mdl.Model Telegram

type RespOrErr = Either String Response

type Param = Option 'Https

instance Bot Telegram where
  prepareModel _ lggr cfg =
    Mdl.Model
      Telegram
      (T.concat ["bot", Cfg.token cfg])
      (Mdl.createHttpConfig $ Cfg.proxy cfg)
      (writeLog lggr)
      (Cfg.helpMessage cfg)
      (Cfg.defRepeatsNumber cfg)
      0
      []
  runBot model@(Mdl.Model _ token httpConfig wrLog helpMsg repNum offset state) = do
    wrLog (Debug, "Sending a request.")
    response <- getUpdates token offset httpConfig
    logResponse wrLog response
    let msgsForAnswer = processMsgs helpMsg repNum state response
    answersNum <- answerForMsgs token httpConfig msgsForAnswer
    wrLog (Debug, show answersNum ++ " messages are sent.")
    runBot $ updateOffset response model

getUpdates :: T.Text -> Int -> HttpConfig -> IO RespOrErr
getUpdates token offset httpCfg = runReq httpCfg getReq
  where
    url = https "api.telegram.org" /: token /: "getUpdates"
    getReq = do
      resp <- req GET url NoReqBody lbsResponse $ "offset" =: offset
      return $ eitherDecode $ Req.responseBody resp

logResponse :: ((LogLevel, String) -> IO ()) -> RespOrErr -> IO ()
logResponse wrLog (Left err) = wrLog (Errors, err)
logResponse wrLog (Right (Response True [] _)) =
  wrLog (Debug, "Server sent updates. There are no updates.")
logResponse wrLog (Right (Response True upds _)) =
  wrLog
    (Debug, "Server sent updates. Got " ++ (show . length) upds ++ " updates.")
logResponse wrLog (Right (Response False _ describe)) =
  wrLog (Errors, "Server sent an error. " ++ fromMaybe "" describe)

answerForMsgs :: T.Text -> HttpConfig -> [Param] -> IO Int
answerForMsgs _ _ [] = return 0
answerForMsgs token httpCfg msgs = runReq httpCfg postReq
  where
    url = https "api.telegram.org" /: token /: "sendMessage"
    postReq = do
      n <- mapM (req POST url NoReqBody ignoreResponse) msgs
      return $ length n

processMsgs :: T.Text -> Int -> Mdl.UserStates -> RespOrErr -> [Param]
processMsgs helpMsg repNum state (Right (Response True updates@(_:_) _)) =
  process $ mapMaybe message updates
  where
    process = map answer
    answer :: Message -> Param
    answer (Message _ _ txt cht usr) =
      "chat_id" =: (chatId cht) <> "text" =:
      case T.head txt == '/' of
        True  -> (recognizeCommand $ T.tail txt)
        False -> plainMessage
      where
        findState u = find ((== userId u) . Mdl.id) state
        plainMessage =
          case usr >>= findState >>= return . Mdl.repNum of
            Just n  -> T.unlines $ replicate n txt
            Nothing -> T.unlines $ replicate repNum txt
        recognizeCommand "help" = helpMsg
        recognizeCommand _      = "Oh, not yet."
processMsgs _ _ _ _ = []

updateOffset :: RespOrErr -> Model -> Model
updateOffset (Right (Response True updates@(u:us) _)) model =
  (\m -> m {Mdl.offset = (updateId . last $ updates) + 1}) model
updateOffset _ model = model
