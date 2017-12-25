{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Service.Log
  ( Handle(..)
  , HasLogHandle(..)
  -- , lInfo
  , module CS
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Logger
import Control.Monad.Logger.CallStack as CS
import RIO

type LogFun = forall msg. ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> IO ()

data Handle =
    Handle { _loggerLog :: LogFun
           }

class HasLogHandle a where
  getLogHandle :: a -> Handle

instance HasLogHandle Handle where
  getLogHandle = id

instance HasLogHandle env => MonadLogger (RIO env) where
  monadLoggerLog loc src level msg = do
    logFun <- asks $ _loggerLog . getLogHandle
    liftIO $ logFun loc src level msg

