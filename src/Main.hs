{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config
import           Data.Text
import           Logger
import           Model
import           System.Environment
import           Telegram           as T

main :: IO ()
main = getArgs >>= processArgs >>= runMainLoop

processArgs [] = return $ Left "A path to a config file is required."
processArgs ("help":_) = return $ Left help
processArgs (configPath:_) = loadConfig configPath

runMainLoop :: Either String Config -> IO ()
runMainLoop (Left msg) = putStrLn msg
runMainLoop (Right config) = logger >>= run
  where
    logger = initLogger (unpack $ logStream config) (logLevel config)
    run (Right logger) = T.run $ modelFromConfig logger config
    run (Left a)       = runMainLoop $ Left a

help = "Ы?)"
