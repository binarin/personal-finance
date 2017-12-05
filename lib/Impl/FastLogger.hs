module Impl.FastLogger (newHandle) where

import Service.Log as SvcLog
import System.Log.FastLogger

newHandle :: IO SvcLog.Handle
newHandle = do
    loggerSet <- newStderrLoggerSet defaultBufSize
    return $ Handle (pushLogStrLn loggerSet . toLogStr)
