{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Bot
import           Config
import           Data.Text
import           Logger
import           Model              hiding (service)
import           System.Environment
import qualified Telegram.Bot       as T
import           Types

main :: IO ()
main = getArgs >>= processArgs >>= runMainLoop

processArgs :: [String] -> IO (Either String Config)
processArgs [] = return $ Left "A path to a config file is required."
processArgs ("help":_) = return $ Left help
processArgs (configPath:_) = loadConfig configPath

runMainLoop :: Either String Config -> IO ()
runMainLoop (Left msg) = putStrLn msg
runMainLoop (Right config) = logger >>= run (service config) config
  where
    logger = initLogger (unpack $ logStream config) (logLevel config)
    run :: Service -> Config -> Either String Logger -> IO ()
    run Telegram cfg (Right logger) =
      runBot (prepareModel logger cfg :: Model Telegram)
    run _ _ (Left err) = runMainLoop $ Left err

help = "Ы?)"
