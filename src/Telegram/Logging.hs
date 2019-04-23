{-# LANGUAGE NamedFieldPuns #-}

module Telegram.Logging where

import           Logger
import           Model
import           Telegram.Types

logRequesting :: TGModel -> IO ()
logRequesting Model {writeLog} = writeLog (Info, "Requesting for updates...")

logProcess :: TGModel -> [Log] -> IO ()
logProcess Model {writeLog} = mapM_ writeLog

logMessages :: TGModel -> Int -> IO ()
logMessages Model {writeLog} number =
  writeLog (Info, show number <> " messages are sent.")
