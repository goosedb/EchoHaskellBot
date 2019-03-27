{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot where

import           Bot
import qualified Config                     as Cfg
import           Control.Monad.State.Strict
import           Data.Aeson
import qualified Data.ByteString            as BS
import           Data.List
import           Data.Maybe
import qualified Data.Text                  as T
import           Logger
import           Model                      as Mdl
import qualified Network.HTTP.Client        as Cl
import           Network.HTTP.Req           as Req
import           Telegram.Types
import           Types                      (Service (..))

type ModelTG = Mdl.Model 'Telegram

type RespOrErr = Either String Response

type Param = Option 'Https

instance Bot 'Telegram where
  prepareModel = modelForTelegram
  runBot model = do
    logStart model
    response <- getUpdates model
    logResponse model response
    runBot $ updateOffset response model

{--==== Http requests ====--}
getUpdates :: ModelTG -> IO RespOrErr
getUpdates Model {token, offset, httpConfig} = runReq httpConfig getReq
  where
    url = https "api.telegram.org" /: token /: "getUpdates"
    getReq = do
      resp <- req GET url NoReqBody lbsResponse $ "offset" =: offset
      return $ eitherDecode $ Req.responseBody resp

postAnswers :: ModelTG -> [Param] -> IO Int
postAnswers _ [] = return 0
postAnswers Model {token, httpConfig} msgs = runReq httpConfig postReq
  where
    url = https "api.telegram.org" /: token /: "sendMessage"
    postReq = do
      n <- mapM (req POST url NoReqBody ignoreResponse) msgs
      return $ length n

{--==== Logging ====--}
logResponse :: ModelTG -> RespOrErr -> IO ()
logResponse Model {logWriter = write} = write . wrLog
  where
    wrLog (Left err) = (Errors, err)
    wrLog (Right (Response True [] _)) = (Debug, msg)
      where
        msg = "Server sent a response. There are no updates."
    wrLog (Right (Response True upds _)) = (Debug, msg)
      where
        msg = "Server sent updates. Got " ++ (show . length) upds ++ " updates."
    wrLog (Right (Response False _ describe)) = (Warnings, msg)
      where
        msg = "Server sent an error. " ++ fromMaybe "" describe

logStart :: ModelTG -> IO ()
logStart Model {logWriter = write} = write (Debug, "Requesting updates...")

{--==== Utils ====--}
updateOffset :: RespOrErr -> ModelTG -> ModelTG
updateOffset (Right Response {result = upds@(_:_)}) model =
  model {offset = (updateId . last $ upds) + 1}
updateOffset _ model = model

modelForTelegram lggr cfg =
  Mdl.Model
    Telegram
    (T.concat ["bot", Cfg.token cfg])
    (Cfg.createHttpConfig $ Cfg.proxy cfg)
    (writeLog lggr)
    (Cfg.helpMessage cfg)
    (Cfg.defRepeatsNumber cfg)
    0
    []