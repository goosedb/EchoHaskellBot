{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Bot where

import           Config                 ()
import           Control.Monad.IO.Class ()
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Logger

type Request m r = ReaderT m IO r

type Process s r = StateT s (Writer [Log]) r

type Bot model = StateT model IO ()

runBot :: StateT model IO () -> model -> IO model
runBot = execStateT

class ServiceBot model where
  bot :: Bot model
