{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
module GUI where

import           Numeric (showFFloat)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
import qualified Database.SQLite.Simple as SQL

import           Config
import           UIStyle (writeCss)
import           RIO
import           Core.Account

import qualified Service.Account as SvcAcc
import qualified Impl.ToshlAccount as SvcAcc

import           Service.Log (HasLogHandle(..), logDebug)
import qualified Service.Log as SvcLog
import qualified Impl.FastLogger as SvcLog

import qualified Service.Bank as SvcBank
import qualified Impl.BankSQLite as SvcBank


class HasWindow env where
    currentWindow :: env -> Window

instance HasWindow env => MonadUI (RIO env) where
    liftUI ui = RIO $ ReaderT $ \env -> runUI (currentWindow env) ui

data Env = Env { _envConfig :: Config
               , _envStylesheet :: FilePath
               , _envSvcAcc :: SvcAcc.Handle
               , _envSvcLog :: SvcLog.Handle
               , _envSvcBank :: SvcBank.Handle
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

class HasAccHandle a where
    getAccHandle :: a -> SvcAcc.Handle

instance HasAccHandle Env where
    getAccHandle env = env^.svcAcc

instance HasAccHandle App where
    getAccHandle (App env _) = getAccHandle env

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
        let jsLogProxy :: C8.ByteString -> IO ()
            jsLogProxy bs = SvcLog._loggerLog logHandle SvcLog.defaultLoc "TP" SvcLog.LevelInfo bs

        svcAccount <- liftIO $ SvcAcc.newHandle $ SvcAcc.Config (ourConfig^.toshlUrl) (ourConfig^.toshlToken) logHandle

        sqliteConn <- managed $ SQL.withConnection "db.sqlite"
        svcBank <- liftIO $ SvcBank.newHandle logHandle sqliteConn

        let env = Env ourConfig cssPath svcAccount logHandle svcBank
        liftIO $ startGUI (tpConfig { jsLog = jsLogProxy }) (setup env)

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


getTransactions :: Account -> Day -> RIO App [Transaction]
getTransactions acc forDay = do
    svc <- asks getAccHandle
    liftIO $ SvcAcc.getTransactions svc acc forDay


formatExpenseAmount :: Int -> String
formatExpenseAmount amt = showFFloat (Just 2) ((fromIntegral amt :: Double) / 100) ""

formatCurrency :: Text -> String
formatCurrency "EUR" = "€"
formatCurrency "RUB" = "₽"
formatCurrency other = T.unpack other


makeAmountElement amount currency = UI.div #+ [UI.string $ formatExpenseAmount amount <> " " <> formatCurrency currency]

trnAmountElt :: Transaction -> UI Element
trnAmountElt (TrExpense exp) = makeAmountElement (- exp^.amount) (exp^.currency)
trnAmountElt (TrIncome inc) = makeAmountElement (inc^.amount) (inc^.currency)
trnAmountElt (TrTransfer xfr) = makeAmountElement (xfr^.amount) (xfr^.fromCurrency)

divWithText :: Text -> UI Element
divWithText txt = UI.string $ T.unpack $ txt

trnCategoryElt :: Transaction -> UI Element
trnCategoryElt (TrExpense exp) = divWithText (exp^.category.name)
trnCategoryElt (TrIncome inc) = divWithText (inc^.category.name)
trnCategoryElt (TrTransfer xfr) = divWithText ("-> " <> xfr^.toAccount.name)

divWithDate :: Day -> UI Element
divWithDate day = UI.string $ yyyy_mm_dd day

trnDateElt :: Transaction -> UI Element
trnDateElt (TrExpense exp) = divWithDate (exp^.day)
trnDateElt (TrIncome inc) = divWithDate (inc^.day)
trnDateElt (TrTransfer xfr) = divWithDate (xfr^.day)

trnTagsElt :: Transaction -> UI Element
trnTagsElt _ = UI.string "tags"

trnDescElts :: Transaction -> UI [(String, Element)]
trnDescElts trn = case description of
    Nothing -> pure []
    Just desc -> do
      descElt <- UI.string $ T.unpack $ desc
      pure [("description", descElt)]
  where
    description = Just "lol"
    -- (TrExpense exp) = (exp^.description)
      -- TrIncome inc -> (inc^.description)
      -- TrTransfer xfr -> (xfr^.description)

showTransaction :: Transaction -> UI Element
showTransaction trn = liftUI $ do
    amountElt <- trnAmountElt trn
    categoryElt <- trnCategoryElt trn
    dayElt <- trnDateElt trn
    tagsElt <- trnTagsElt trn
    descElts <- trnDescElts trn
    block blockName $ [("category", categoryElt)
                      ,("amount", amountElt)
                      ,("date", dayElt)
                      ,("tags", tagsElt)
                      ] ++ descElts
  where
    blockName = case trn of
      TrExpense _ -> "expense"
      TrIncome _ -> "income"
      TrTransfer _ -> "transfer"


childrenM :: WriteAttr Element [UI Element]
childrenM = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ i

newToshlEntries :: Account -> Tidings Day -> RIO App Element
newToshlEntries _acc dayT = do
  initialDay <- liftIO $ currentValue (facts dayT)
  trns <- getTransactions (Account "abn" "EUR") (pred (pred (pred initialDay)))
  (modifyTrnsEv, modifyTrns) <- liftIO $ newEvent
  trnsB <- liftIO $ accumB trns modifyTrnsEv
  env <- ask
  liftUI $ onEvent (rumors dayT) $ \newDay -> do
    trns <- liftIO $ runRIO env $ getTransactions (Account "abn" "EUR") newDay
    liftIO $ modifyTrns $ const trns
    pure ()
  liftUI $ do
    container <- UI.div
    element container # sink childrenM (map <$> pure showTransaction <*> trnsB)
    pure container

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
