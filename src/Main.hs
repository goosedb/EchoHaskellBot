{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config
import           Data.Text
import           Logger
import           State
import           System.Environment

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
    run (Right logger) = mainLoop $ stateFromConfig logger config
    run (Left a)       = runMainLoop $ Left a

mainLoop :: State -> IO ()
mainLoop state = undefined

help = "Ы?)"
