{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
module GUI where


import           Data.Time.Format (formatTime, parseTimeM, defaultTimeLocale)
import           Data.Time.LocalTime (getZonedTime, zonedTimeToLocalTime, localDay)
import           Data.Time.Calendar (Day)
import           Data.Monoid ((<>))
import           Control.Concurrent.STM.TVar (newTVar)
import           Control.Monad.STM (atomically)
import           Data.Time.Calendar (fromGregorian)
import           Data.Monoid (Last(..))
import qualified Data.ByteString.Char8 as C8
import           System.Exit (die)
import           Control.Lens hiding ((#), set, element, children)
import           System.Environment (lookupEnv)
import           Control.Monad.Managed
import           Control.Monad.Catch
import           Control.Monad (void, forM_)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Core as UI
import           Graphics.UI.Threepenny.Core hiding (Config, row, column, newEvent)
import           Graphics.UI.Threepenny.JQuery
import qualified Graphics.UI.Threepenny.Widgets as UI
import           System.IO.Temp (emptySystemTempFile)
import           Data.Default
import           System.Directory (removeFile)

import           Config
import           UIStyle (writeCss)
import           RIO
import           Core.Account

import qualified Service.Account as SvcAcc
import qualified Impl.ToshlAccount as SvcAcc

import           Service.Log (HasLogHandle(..), lInfo)
import qualified Service.Log as SvcLog
import qualified Impl.FastLogger as SvcLog


class HasWindow env where
    currentWindow :: env -> Window

instance HasWindow env => MonadUI (RIO env) where
    liftUI ui = RIO $ ReaderT $ \env -> runUI (currentWindow env) ui

data Env = Env { _envConfig :: Config
               , _envStylesheet :: FilePath
               , _envSvcAcc :: SvcAcc.Handle
               , _envSvcLog :: SvcLog.Handle
               }
makeFields ''Env

instance HasLogHandle Env where
    getLogHandle = _envSvcLog

data App = App Env Window
instance HasWindow App where
    currentWindow (App _ window) = window
instance HasStylesheet App FilePath where
    stylesheet k (App env window) = fmap (\newS -> App (env & stylesheet .~ newS) window) (k (env^.stylesheet))

instance HasLogHandle App where
    getLogHandle (App env _) = getLogHandle env

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

        logHandle <- managed $ SvcLog.withHandle
        svcAccount <- liftIO $ SvcAcc.newHandle $ SvcAcc.Config (ourConfig^.toshlUrl) (ourConfig^.toshlToken) logHandle

        let env = Env ourConfig cssPath svcAccount logHandle
        -- liftIO $ SvcAcc.insertTransaction svcAccount sample
        liftIO $ startGUI tpConfig (setup env)

sample :: Transaction
sample = TrExpense $ Expense 1000 (Account "abn" "EUR") ("EUR") (Category "Еда и напитки" ExpenseCategory) [Tag "binarin", Tag "marina"] (fromGregorian 2017 12 03) Nothing


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
    entry <- reconcillationUI
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

keypress :: Element -> Event Char
keypress = fmap (toEnum . read . head . unsafeFromJSON) . domEvent "keypress"

row :: MonadUI m => [m Element] -> m Element
row elts = do
    eltsPure <- sequence elts
    container <- liftUI $ UI.row (pure <$> eltsPure)
    pure container

column :: MonadUI m => [m Element] -> m Element
column elts = do
    eltsPure <- sequence elts
    container <- liftUI $ UI.column (pure <$> eltsPure)
    pure container

newEvent :: MonadIO m => m (Event a, UI.Handler a)
newEvent = liftIO $ UI.newEvent

block :: MonadUI m => String -> [(String, Element)] -> m Element
block blockName namedElements = do
    container <- liftUI $ UI.div # set UI.class_ blockName
    wrappedElements <- flip mapM namedElements $ \(elName, elt) -> do
        wrapper <- liftUI $ UI.div # set UI.class_ (blockName <> "__" <> elName)
                                   # set children [elt]
        pure wrapper
    liftUI $ pure container # set children wrappedElements


reconcillationUI :: RIO App Element
reconcillationUI = do
    (dateWidget, dateTidings) <- newDateWidget
    toshlEntries <- newToshlEntries (Account "abn" "EUR") dateTidings
    bankEntries <- newBankEntries
    entryEditor <- expenseEntry
    block "reconcillation" [("date", dateWidget)
                           ,("toshl", toshlEntries)
                           ,("bank", bankEntries)
                           ,("editor", entryEditor)
                           ]

yyyy_mm_dd :: Day -> String
yyyy_mm_dd = formatTime defaultTimeLocale "%Y-%m-%d"

parseDay :: String -> Day
parseDay str = maybe (fromGregorian 2011 1 1) id parsed
  where
    parsed = parseTimeM False defaultTimeLocale "%Y-%m-%d" str

newDateWidget :: RIO App (Element, Tidings Day)
newDateWidget = do
    defaultDate <- (localDay . zonedTimeToLocalTime) <$> liftIO getZonedTime

    entry' <- liftUI $ UI.input # set UI.type_ "date"
                                # set UI.value (yyyy_mm_dd defaultDate)
                                # set (UI.attr "required") "1"
    prevBtn <- liftUI $ UI.button # set UI.text "<"
    nextBtn <- liftUI $ UI.button # set UI.text ">"
    let setEvent :: Event (Day -> Day) = (\newValue -> const $ parseDay newValue) <$> UI.valueChange entry'
    let prevEvent :: Event (Day -> Day) = const pred <$> UI.click prevBtn
    let nextEvent :: Event (Day -> Day) = const succ <$> UI.click nextBtn
    let combinedEvent :: Event (Day -> Day) = concatenate <$> unions [setEvent, prevEvent, nextEvent]
    dayBehaviour <- accumB defaultDate combinedEvent
    dayEvents <- accumE defaultDate combinedEvent
    void $ liftUI $ sink UI.value (yyyy_mm_dd <$> dayBehaviour) (pure entry')

    elt <- block "date" [("prev", prevBtn), ("date", entry'), ("next", nextBtn)]
    pure (elt, tidings dayBehaviour dayEvents)

newToshlEntries :: Account -> Tidings Day -> RIO App Element
newToshlEntries _acc _dayT = do
    liftUI $ UI.string "toshl entries"

newBankEntries :: RIO App Element
newBankEntries = do
    liftUI $ UI.string "bank entries"

expenseEntry :: RIO App Element
expenseEntry = do
    liftUI $ do
        dateInput <- UI.input # set UI.type_ "date"
        amount <- UI.input
        (buttons, buttonClick) <- radioButtons [("a", 1), ("b", 2)]
        on UI.valueChange amount $ \val -> liftIO (putStrLn val)
        onEvent buttonClick $ \val -> liftIO (putStrLn $ show val)
        container <- column [pure dateInput, pure amount, pure buttons]
        on keypress container (\k -> liftIO (putStrLn $ show k))
        pure container

radioButtons :: [(String, a)] -> UI (Element, Event a)
radioButtons buttons = do
    (event, handler) <- liftIO $ UI.newEvent
    container <- UI.div # set UI.class_ "radio"
    forM_ buttons $ \(label, value) -> do
        button <- UI.button # set UI.text label
        on UI.click button $ \() -> liftIO $ handler value
        pure container #+ [pure button]
        return ()

    return (container, event)
