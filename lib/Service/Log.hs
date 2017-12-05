{-# LANGUAGE RankNTypes #-}
module Service.Log
  ( Handle(..)
  , HasLogHandle(..)
  , lInfo
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import System.Log.FastLogger (ToLogStr)

import RIO

data Handle =
    Handle { _lInfo :: forall a. ToLogStr a => a -> IO ()
           }

class HasLogHandle a where
  getLogHandle :: a -> Handle

instance HasLogHandle Handle where
  getLogHandle = id

lInfo :: (ToLogStr a, HasLogHandle env) => a -> RIO env ()
lInfo str = do
    logHandle <- asks getLogHandle
    liftIO $ _lInfo logHandle str
