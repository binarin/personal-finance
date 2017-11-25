{-# LANGUAGE ScopedTypeVariables #-}
module UI where

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           System.Process (readProcess, callProcess)
import           Control.Exception (try, SomeException)
import           Control.Concurrent (threadDelay, forkIO)
import           Control.Monad (forM_, void)
import Text.Read (readMaybe)

mkConfig :: IO Graphics.UI.Threepenny.Core.Config
mkConfig = return defaultConfig { jsPort = Just 8024
                                , jsStatic = Just "./static"
                                , jsWindowReloadOnDisconnect = True
                                , jsCustomHTML = Just "index.html"
                                }
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
      threadDelay 1000000
      kickFirefox
  main

main :: IO ()
main = do
    cfg <- mkConfig
    startGUI cfg setup

setup :: Window -> UI ()
setup window = do
    return window # set UI.title "Hello, World!!!!"
    return ()
