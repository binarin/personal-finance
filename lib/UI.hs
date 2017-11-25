{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module UI where

import           Data.Monoid (Last(..))
import qualified Data.ByteString.Char8 as C8
import           System.Exit (die)
import           Control.Lens hiding ((#), set, element, children)
import           System.Environment (lookupEnv)
import           Control.Monad.Managed
import           Control.Monad.Catch
import           Control.Monad (void)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Core as UI
import           Graphics.UI.Threepenny.Core hiding (Config)
import           Graphics.UI.Threepenny.JQuery
import qualified Graphics.UI.Threepenny.Widgets as UI
import           System.IO.Temp (withSystemTempFile, emptySystemTempFile)
import           Data.Default
import           System.Directory (removeFile)

import           Config
import           UIStyle (writeCss)

import qualified Service.Account as SvcAcc
import qualified Impl.ToshlAccount as SvcAcc

newtype RIO env a = RIO { unRIO :: ReaderT env IO a }
  deriving (Functor,Applicative,Monad,MonadIO,MonadReader env)

runRIO :: MonadIO m => env -> RIO env a -> m a
runRIO env (RIO (ReaderT f)) = liftIO (f env)

class HasWindow env where
    currentWindow :: env -> Window

instance HasWindow env => MonadUI (RIO env) where
    liftUI ui = RIO $ ReaderT $ \env -> runUI (currentWindow env) ui

data Env = Env { _envConfig :: Config
               , _envStylesheet :: FilePath
               , _envSvcAcc :: SvcAcc.Handle
               }
makeFields ''Env

data App = App Env Window
instance HasWindow App where
    currentWindow (App _ window) = window
instance HasStylesheet App FilePath where
    stylesheet k (App env window) = fmap (\newS -> App (env & stylesheet .~ newS) window) (k (env^.stylesheet))

parseEnvConfig :: IO PartialConfig
parseEnvConfig = do
    url <- lookupEnv "TOSHL_URL"
    token <- lookupEnv "TOSHL_TOKEN"
    return $ def & toshlUrl .~ (Last $ C8.pack <$> url)
                 & toshlToken .~ (Last $ C8.pack <$> token)

main :: IO ()
main = do
    envConfig <- parseEnvConfig
    ourConfig <- either die return $ makeConfig envConfig

    tpConfig <- makeThreepennyConfig
    runManaged $ do
        cssPath <- managed $ withTempPath "finance.css"
        liftIO $ writeCss cssPath

        let svcAccount = SvcAcc.newHandle SvcAcc.Config { SvcAcc.url = (ourConfig^.toshlUrl)
                                                        , SvcAcc.token = (ourConfig^.toshlToken) }
        let env = Env ourConfig cssPath svcAccount

        liftIO $ startGUI tpConfig (setup env)

withTempPath :: String -> (FilePath -> IO a) -> IO a
withTempPath template action = bracket (emptySystemTempFile template) removeFile action

injectCustomCss :: RIO App ()
injectCustomCss = void $ do
    path <- view stylesheet
    url <- liftUI $ loadFile "text/css" path
    el <- liftUI $ mkElement "link" # set (attr "rel") "stylesheet"
                                    # set (attr "type") "text/css"
                                    # set (attr "href") url
    w <- asks (\(App _ w) -> w)
    liftUI $ getHead w #+ [element el]

setup :: Env -> Window -> UI ()
setup env window = do
    let app = App env window
    liftIO $ runRIO app $ setupApp

setupApp :: RIO App ()
setupApp = do
    injectCustomCss
    withWindowU $ set UI.title "Hello, World!!!!"
    entry <- expenseEntry
    setContent [entry]
    return ()

withWindow :: (Window -> UI a) -> RIO App ()
withWindow ui = do
    App _ window <- ask
    void $ liftUI $ ui window
    return ()

withWindowU :: (UI Window -> UI a) -> RIO App ()
withWindowU act = withWindow $ act . pure

setContent :: [Element] -> RIO App ()
setContent elts = do
    withWindow $ \w -> getBody w & set children elts

makeThreepennyConfig :: IO UI.Config
makeThreepennyConfig = return defaultConfig { jsPort = Just 8024
                                , jsStatic = Just "./static"
                                , jsWindowReloadOnDisconnect = True
                                , jsCustomHTML = Just "index.html"
                                }

expenseEntry :: RIO App Element
expenseEntry = do
    liftUI $ do
        amount <- UI.input
        on UI.valueChange amount $ \val -> liftIO (putStrLn val)
        column [pure amount]
