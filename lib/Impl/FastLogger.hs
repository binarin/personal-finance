module Impl.FastLogger (withHandle) where

import Data.Monoid ((<>))
import Service.Log as SvcLog
import System.Log.FastLogger

withHandle :: (SvcLog.Handle -> IO a) -> IO a
withHandle action = withFastLogger (LogStderr defaultBufSize) $ \fl -> do
    action $ Handle (\msg -> fl $ toLogStr msg <> "\n")
