{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot where

import           Bot
import qualified Config                     as Cfg
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Control.Monad.Writer
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
import           Types                      (Service (..), UserState,
                                             UserStates)

type ModelTG = Mdl.Model 'Telegram

type RespOrErr = Either String Response

type Param = Option 'Https

instance Bot 'Telegram where
  prepareModel = modelForTelegram
  runBot model = do
    logStart model
    response <- getUpdates model
    logResponse model response
    when
      (isRight response)
      (do let resp = fromRight response
          let ((messages, (model', _)), log) =
                runWriter $ runStateT processResponse (model, resp)
          postAnswers model' messages
          return ())
    runBot $ updateOffset response model

fromRight (Right a) = a

isRight (Right _) = True
isRight (Left _)  = False

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

processResponse :: StateT (ModelTG, Response) (Writer [String]) [Param]
processResponse = do
  isOk <- gets (ok . snd)
  if not isOk
    then do
      descOfErr <- gets (description . snd)
      tell $ ["Received not-ok response. " <> (fromMaybe "" descOfErr)]
      return []
    else do
      (states, resp) <- get
      let (messages, (newStates, _)) =
            runState processUpdates (states, result resp)
      put (newStates, resp)
      return messages

processUpdates :: State (ModelTG, [Update]) [Param]
processUpdates = do
  upds <- gets snd
  if not $ null upds
    then do
      (states, _) <- get
      let (u:us) = upds
      let (message, (states', _)) = runState generateMessage (states, u)
      put (states', us)
      others <- processUpdates
      return $ message : others
    else return []

generateMessage :: State (ModelTG, Update) Param
generateMessage = do
  message <- gets (message . snd)
  if isJust message
    then do
      let msg = fromJust message
      return $ "chat_id" =: (chatId . chat $ msg) <> "text" =: (text msg)
    else return mempty

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
    (createWriter lggr)
    (Cfg.helpMessage cfg)
    (Cfg.defRepeatsNumber cfg)
    0
    []
