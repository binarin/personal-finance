module Impl.BankABNText where

import           Text.Regex.Posix
import           Data.Monoid
import qualified Control.Monad.Logger.CallStack as LogCS
import           Control.Monad.Reader
import           GHC.Stack
import           Control.Lens
import           Control.Monad
import           System.Directory (listDirectory)
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import           Service.Bank as Svc
import qualified Service.Log as SvcLog
import           Data.Time.Calendar (Day)
import qualified Data.ByteString.Lazy as BL

import           Core.Bank
import           Parser.ABNTab (parse)
import           RIO

data IConfig = IConfig { _iConfigLogger :: SvcLog.Handle
                       , _iConfigTransactions :: M.Map Day [BankTrn]
                       }

logInfo :: SvcLog.Handle -> (HasCallStack => Text -> IO ())
logInfo logImpl = \text -> runRIO logImpl $ LogCS.logInfo text


newHandle :: SvcLog.Handle -> FilePath -> IO Svc.Handle
newHandle logImpl reportsDir = do
  logInfo logImpl $ "Loading reports from " <> T.pack reportsDir
  dayData <- loadStatements reportsDir
  forM_ (M.keys dayData) $ \whichDay -> do
    logInfo logImpl $ "Got data for " <> T.pack (show whichDay) <> ": " <> T.pack (show $ length $ fromMaybe [] $ M.lookup whichDay dayData)
  pure Svc.Handle { _saveStatement = \_ _ -> pure $ Left "no-op"
                  , _getTransactions = \whichDay -> pure $ fromMaybe [] (M.lookup whichDay dayData)
                  }

loadStatements :: FilePath -> IO (M.Map Day [BankTrn])
loadStatements path = do
  files <- listDirectory path
  foldM (getFromFile path) M.empty (filter looksLikeReport files)
  where
    looksLikeReport file = file =~ ("\\.TAB$" :: String)


getFromFile :: FilePath -> M.Map Day [BankTrn] -> FilePath -> IO (M.Map Day [BankTrn])
getFromFile dir perDay path = do
  content <- BL.readFile $ dir <> "/" <> path
  case parse content of
    Left _ -> pure perDay
    Right rows -> pure $ foldl addRow perDay rows

type PerDay = M.Map Day [BankTrn]

addRow :: PerDay -> BankTrn -> PerDay
addRow perDay trn = M.alter addTrn (trn^.day) perDay
  where
    addTrn Nothing = Just [trn]
    addTrn (Just trns) = Just $ trn:trns
