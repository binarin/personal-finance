{-# LANGUAGE ScopedTypeVariables #-}
module UILive where

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void, forM_)
import Control.Monad.Catch
import System.Process (readProcess, callProcess)
import Text.Read (readMaybe)

import qualified UI

kickFirefox :: IO ()
kickFirefox = do
    maybeWindows :: Either SomeException String <- try $ readProcess "xdotool" ["search", "--all", "--name", "Problem loading"] ""
    case maybeWindows of
      Left _ -> return ()
      Right idStr -> forM_ (lines idStr) $ \windowId ->
        case readMaybe windowId of
          Nothing -> return ()
          Just (n :: Integer) -> do
              putStrLn $ show n
              callProcess "xdotool" ["key", "--window", show n, "CTRL+R"]
              return ()

live :: IO ()
live = do
  void $ forkIO $ do
      threadDelay 200000
      kickFirefox
  UI.main
