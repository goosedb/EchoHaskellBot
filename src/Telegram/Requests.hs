{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram.Requests where

import           Control.Monad.Reader
import           Data.ByteString.Char8 (pack)
import           Data.Maybe
import           Logger
import           Model
import           Network.HTTP.Simple
import           Telegram.Types
import           Telegram.Utils

getUpdates :: TGRequest (TGResponse Updates)
getUpdates = do
  model <- ask
  let write = writeLog model
  lift $ write (Info, "Requesting for updates..")
  url <- parseRequest $ defUrl model "/getUpdates"
  let offset' = offset . serviceData $ model
  let request =
        setRequestMethod "GET" $
        setRequestQueryString [("offset", Just $ pack $ show offset')] $
        setRequestProxy (Model.proxy model) url
  response <- lift $ httpJSON request
  return $ getResponseBody response

sendMessages :: [MessageToSend] -> TGRequest ()
sendMessages msgs = do
  model <- ask
  let write = writeLog model
  unless (null msgs) (lift $ write (Info, "Sending answers.."))
  url <- parseRequest $ defUrl model "/sendMessage"
  let buildRequest m =
        setRequestMethod "POST" $
        setRequestProxy (Model.proxy model) $ setRequestBodyJSON m url
  let requests = map buildRequest msgs
  result <- lift $ mapM httpJSON requests
  let bodies = map getResponseBody result
  logFeedback write bodies
  return ()
  where
    descs :: [TGResponse Message] -> String
    descs = concatMap (fromMaybe "" . responseDescription)
    logFeedback _ [] = return ()
    logFeedback write bs =
      let notOkResp = filter (not . responseOk) bs
       in lift . write $
          if | null notOkResp -> (Info, "All answers are sent successfully")
             | otherwise -> (Warnings, "Some answers wasn't sent:" <> descs bs)
