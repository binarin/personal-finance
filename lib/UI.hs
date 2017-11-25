{-# LANGUAGE ScopedTypeVariables #-}
module UI where

import           System.IO (hClose)
import           System.IO.Temp (withSystemTempFile)
import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Widgets as UI
import           Graphics.UI.Threepenny.Core

import           System.Process (readProcess, callProcess)
import           Control.Exception (try, SomeException)
import           Control.Concurrent (threadDelay, forkIO)
import           Control.Monad (forM_, void)
import           Text.Read (readMaybe)

import           UIStyle (writeCss)

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
      threadDelay 200000
      kickFirefox
  main

main :: IO ()
main = do
    cfg <- mkConfig
    withSystemTempFile "finance.css" $ \stylesheet handle -> do
        hClose handle
        writeCss stylesheet
        startGUI cfg (setup stylesheet)

injectCustomCss :: Window -> FilePath -> UI ()
injectCustomCss w path = void $ do
    url <- loadFile "text/css" path
    el <- mkElement "link" # set (attr "rel") "stylesheet"
                           # set (attr "type") "text/css"
                           # set (attr "href") url
    getHead w #+ [element el]


setup :: FilePath -> Window -> UI ()
setup cssPath window = do
    injectCustomCss window cssPath
    return window # set UI.title "Hello, World!!!!"
    input <- UI.input
    getBody window #+ [element input]
    return ()
