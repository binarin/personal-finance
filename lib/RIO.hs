{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RIO where

import Control.Monad.IO.Class
import Control.Monad.Reader

newtype RIO env a = RIO { unRIO :: ReaderT env IO a }
  deriving (Functor,Applicative,Monad,MonadIO,MonadReader env)

runRIO :: MonadIO m => env -> RIO env a -> m a
runRIO env (RIO (ReaderT f)) = liftIO (f env)
