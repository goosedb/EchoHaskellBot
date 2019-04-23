{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Telegram.Bot where

import           Bot
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Aeson
import           Data.Aeson.Text
import           Data.List
import           Data.Maybe
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as LT
import           Logger
import           Model
import           Telegram.Logging

import           Data.Aeson
import           Model
import           Network.HTTP.Client.Conduit
import           Network.HTTP.Conduit
import           Network.HTTP.Simple         as S
import           Telegram.Requests
import           Telegram.Types              as TGT
import           Telegram.Types
import           Telegram.Utils

runRequest :: TGRequest r -> TGModel -> IO r
runRequest request env = runReaderT request env

runProcess :: Process s r -> s -> (r, s, [Log])
runProcess process state =
  let ((a, s), w) = runWriter $ runStateT process state
   in (a, s, w)

instance ServiceBot TGModel where
  bot = do
    model <- get
    result <- lift $ runRequest getUpdates model
    let (model', msgs, log) = process result model
    lift $ logProcess model' log
    lift $ runRequest (sendMessages msgs) model'
    put model'

process ::
     TGResponse [GetUpdate] -> TGModel -> (TGModel, [MessageToSend], [Log])
process resp model =
  let (msgs, (_, model'), log) = runProcess processResponse (resp, model)
   in (model', msgs, log)

{-_ == Response == _-}
processResponse :: Process (TGResponse [GetUpdate], TGModel) [MessageToSend]
processResponse = do
  isOk <- gets (responseOk . fst)
  if | isOk -> okResponse
     | otherwise -> notOkResponse

okResponse :: Process (TGResponse [GetUpdate], TGModel) [MessageToSend]
okResponse = do
  upds <- gets (fromMaybe [] . responseResult . fst)
  tell $ pure (Info, "Got " <> show (length upds) <> " updates.")
  model <- gets snd
  let result = runProcess processUpdates (upds, model)
  let (msgs, (_, model'), log) = result
  tell log
  modify (\(a, _) -> (a, model'))
  return msgs

notOkResponse :: Process (TGResponse [GetUpdate], TGModel) [MessageToSend]
notOkResponse = do
  description <- gets (responseDescription . fst)
  tell $ pure $ notOkMsg description
  return []
  where
    notOkMsgHead = "Server sent an error. Description: "
    notOkMsgDesc = fromMaybe "no description"
    notOkMsg d = (Warnings, notOkMsgHead <> notOkMsgDesc d <> ".")

{-_ == Updates == _-}
processUpdates :: Process ([GetUpdate], TGModel) [MessageToSend]
processUpdates = do
  upds <- gets fst
  model <- gets snd
  case length upds of
    0 -> return []
    1 -> oneUpdate
    _ -> manyUpdates

manyUpdates :: Process ([GetUpdate], TGModel) [MessageToSend]
manyUpdates = do
  (upds, model) <- get
  let (u:us) = upds
  let (msg, (_, model'), log) = runProcess processUpdate (u, model)
  tell log
  put (us, model')
  msgs <- processUpdates
  return $ msg <> msgs

oneUpdate :: Process ([GetUpdate], TGModel) [MessageToSend]
oneUpdate = do
  (upds, model) <- get
  let [u] = upds
  let (msg, (_, model'), log) = runProcess processUpdate (u, model)
  tell log
  modify (\(a, b) -> (a, updateOffset model' u))
  return msg

processUpdate :: Process (GetUpdate, TGModel) [MessageToSend]
processUpdate = do
  (upd, model) <- get
  let cb = getUpdateCallbackQuery upd
  let msg = getUpdateMessage upd
  if | isJust $ cb -> processCallback
     | isJust $ msg -> processMessage
     | otherwise ->
       do tell $ pure (Debug, "Empty update.")
          return []

{-_ == Callback == _-}
processCallback :: Process (GetUpdate, TGModel) [MessageToSend]
processCallback = do
  cb <- gets (fromJust . getUpdateCallbackQuery . fst)
  model <- gets snd
  let newNumOfRep = parseCallbackData (callbackQueryData cb) model
  let usrId = userId $ callbackQueryFrom cb
  tell $ pure (Debug, cbLog usrId newNumOfRep)
  let newState = UserState (userId . callbackQueryFrom $ cb) newNumOfRep
  modify $ updateStates newState
  model <- gets snd
  tell $ pure (Debug, "New states" <> show (userStates model))
  if | (isJust $ callbackQueryMessage cb) ->
       do let id = (chatId $ messageChat $ fromJust $ callbackQueryMessage cb)
          return $ pure $ newPlainMessage id $ cbMsg newNumOfRep
     | otherwise -> return []
  where
    cbLog id n =
      "For user with id " <> show id <> " set " <> show n <> " repeats."
    cbMsg n = "Now I repeat your message " <> T.pack (show n) <> " times."


{-_ == Message == _-}
processMessage :: Process (GetUpdate, TGModel) [MessageToSend]
processMessage = do
  msg <- gets (fromJust . getUpdateMessage . fst)
  model <- gets snd
  let text = messageText msg
  if | "/" `T.isPrefixOf` text -> command
     | otherwise -> plainMessage

command :: Process (GetUpdate, TGModel) [MessageToSend]
command = do
  msg <- gets (fromJust . getUpdateMessage . fst)
  let text = messageText $ msg
  model <- gets snd
  let id = chatId $ messageChat msg
  tell $ pure (Debug, "Command: " <> T.unpack text)
  case text of
    "/help" -> do
      let msg = helpMsg . defSettings $ model
      return $ pure $ newPlainMessage id msg
    "/repeat" -> do
      let msg = newKeyboardMessage id
      return $ pure $ msg
    _ -> return $ pure $ newPlainMessage id errMsg
  where
    errMsg = "There is no such command. Try /help or /repeat."

plainMessage :: Process (GetUpdate, TGModel) [MessageToSend]
plainMessage = do
  msg <- gets (fromJust . getUpdateMessage . fst)
  let text = messageText $ msg
  model <- gets snd
  let defSet = defSettings model
  let id = chatId $ messageChat msg
  let usrId = userId $ messageFrom msg
  let state = fromMaybe (defState defSet) $ find (p usrId) (userStates model)
  tell $ pure (Debug, "Message: " <> T.unpack text)
  let msg = messageFromState id state text
  return $ pure $ messageFromState id state text
  where
    messageFromState id UserState {stateNumOfRep = n} txt =
      newPlainMessage id $ T.unlines $ replicate n txt
    defState DefaultSettings {numOfRep} = UserState (-1) numOfRep
    p id s2 = id == stateUserId s2
