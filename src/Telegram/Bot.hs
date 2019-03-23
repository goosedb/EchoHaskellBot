{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot where

import           Bot
import qualified Config              as Cfg
import           Data.Aeson
import qualified Data.ByteString     as BS
import           Data.Maybe
import qualified Data.Text           as T
import           Logger
import qualified Model               as Mdl
import qualified Network.HTTP.Client as Cl
import           Network.HTTP.Req    as Req
import           Telegram.Types
import           Web.Internal.HttpApiData

data Telegram =
  Telegram

type Model = Mdl.Model Telegram

type RespOrErr = Either String Response

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
    wrLog (Debug, show answersNum ++ " messages is sent")
    runBot $ updateOffset response model

getUpdates :: T.Text -> Int -> HttpConfig -> IO RespOrErr
getUpdates token offset httpCfg = runReq httpCfg getReq
  where
    url = (https "api.telegram.org" /: token /: "getUpdates")
    getReq = do
      resp <- req GET url NoReqBody lbsResponse $ "offset" =: offset
      return $ eitherDecode $ Req.responseBody resp

logResponse :: LogWriter -> RespOrErr -> IO ()
logResponse wrLog (Left err) = wrLog (Errors, err)
logResponse wrLog (Right (Response True [] _)) =
  wrLog (Debug, "Server sent updates. There are no updates.")
logResponse wrLog (Right (Response True upds _)) =
  wrLog
    (Debug, "Server sent updates. Got " ++ (show . length) upds ++ " updates.")
logResponse wrLog (Right (Response False _ describe)) =
  wrLog (Errors, "Server sent an error. " ++ fromMaybe "" describe)

logAnswers :: Int -> IO ()
logAnswers = undefined

answerForMsgs _ _ [] = return 0
answerForMsgs token httpCfg msgs = runReq httpCfg postReq where
  url = https "api.telegram.org" /: token /: "sendMessage"
  buildAnswer msgs = concat $ map (\(opts, n) -> take n $ repeat opts) msgs
  postReq = do 
    n <- mapM (\answer -> req POST url NoReqBody ignoreResponse answer) $ buildAnswer msgs 
    return $ length n

processMsgs helpMsg repNum state (Right (Response True updates@(_:_) _)) =
  process $ catMaybes $ map message updates
  where
    process []     = []
    process (m:ms) = (answer m) : process ms
    answer (Message _ _ txt cht) =
      (("chat_id" =: (chatId cht) <> "text" =: txt), repNum)
processMsgs _ _ _ _ = []

updateOffset :: Either String Response -> Model -> Model
updateOffset (Right (Response True updates@(u:us) _)) model =
  (\m -> m {Mdl.offset = (updateId . last $ updates) + 1}) model
updateOffset _ model = model
