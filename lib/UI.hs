module UI where

import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core

mkConfig :: IO Graphics.UI.Threepenny.Core.Config
mkConfig = return defaultConfig { jsPort = Just 8023
                                , jsStatic = Just "./static"
                                }

main :: IO ()
main = do
    cfg <- mkConfig
    startGUI cfg setup

setup :: Window -> UI ()
setup window = do
    return window # set UI.title "Hello, World!"
    return ()
