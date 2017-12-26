{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Impl.BankSQLite where

import           Data.Monoid
import           Control.Lens
import           Control.Monad (mapM_)
import qualified Crypto.Hash as Hash
import           Data.ByteString (ByteString)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Calendar (Day, fromGregorian)
import           Database.SQLite.Simple as SQL
import           Text.RawString.QQ

import           Core.Bank
import           RIO
import           Service.Bank as Svc
import qualified Service.Log as SvcLog
import           Service.Log hiding (Handle)

data IConfig = IConfig
  { _iConfigLogger :: SvcLog.Handle
  , _iConfigConnection :: SQL.Connection
  }
makeFields ''IConfig

instance SvcLog.HasLogHandle IConfig where
  getLogHandle ic = ic^.logger

instance Svc.HasBankHandle IConfig where
  getBankHandle ic = Svc.Handle (\parser statement -> runRIO ic $ saveStatement parser statement)

createSql :: Text
createSql = [r|
CREATE TABLE IF NOT EXISTS bank_statements
( id
, source_name
, source_hash
, date_from
, date_to
);

CREATE TABLE IF NOT EXISTS bank_entries
( id
, statement_id
, amount
, currency
, day
, account
, description
)

CREATE TABLE IF NOT EXISTS bank_matches
( bank_entry_id
, transaction_id
)

|]


newHandle :: SvcLog.Handle -> SQL.Connection -> IO Svc.Handle
newHandle logImpl conn = do
  runRIO env $ logInfo "Initialized bank connection"
  pure $ Svc.getBankHandle env
    where
      env = IConfig { _iConfigLogger = logImpl
                    , _iConfigConnection = conn
                    }

calcStatementHash :: StatementName -> StatementContent -> Text
calcStatementHash name content = T.pack $ show (Hash.hash $ nameHash <> contentHash :: Hash.Digest Hash.SHA256)
  where
    nameHash = T.encodeUtf8 $ T.pack $ show (Hash.hash $ T.encodeUtf8 name :: Hash.Digest Hash.SHA256)
    contentHash = T.encodeUtf8 $ calcContentHash content

calcContentHash :: StatementContent -> StatementHash
calcContentHash content = T.pack $ show (Hash.hash content :: Hash.Digest Hash.SHA256)

compoundHash :: [Text] -> Text
compoundHash chunks = T.pack $ show (Hash.hash $ mconcat $ hashPart <$> chunks :: Hash.Digest Hash.SHA256)
  where
    hashPart txt = T.encodeUtf8 $ T.pack $ show (Hash.hash $ T.encodeUtf8 txt :: Hash.Digest Hash.SHA256)

saveStatementImpl :: StatementParserFun
                  -> StatementName
                  -> StatementContent
                  -> RIO IConfig [BankTrn]
saveStatementImpl parser name content =
  parser content & either failWithParseError ensureStatement
 where
  ensureStatement :: [ParsedBankRow] -> RIO IConfig [BankTrn]
  ensureStatement entries = do
    existingTrns <- getStatementTransactions statementNewId
    case existingTrns of
      [] -> insertNewStatement entries
      _ -> pure =<< assertCompatible entries existingTrns

  insertNewStatement :: [ParsedBankRow] -> RIO IConfig [BankTrn]
  insertNewStatement entries = do
    insertStatementToDb statementNew
    let trnsToInsert = toBankTrn <$> entries
    mapM_ insertTrnToDb trnsToInsert
    pure trnsToInsert

  assertCompatible :: [ParsedBankRow] -> [BankTrn] -> RIO IConfig [BankTrn]
  assertCompatible parsed existing = undefined

  mkBankTrnId :: ParsedBankRow -> DBId BankTrn
  mkBankTrnId row = DBId $ compoundHash
   [ statementNewHash
   , T.pack $ show $ row^.rowNumber
   , T.pack $ show $ row^.amount
   , T.pack $ show $ row^.currency
   , T.pack $ show $ row^.day
   , T.pack $ show $ row^.accountNumber
   , T.pack $ show $ row^.description
   ]

  toBankTrn :: ParsedBankRow -> BankTrn
  toBankTrn row = BankTrn { _bankTrnId            = mkBankTrnId row
                          , _bankTrnStatement     = statementNew
                          , _bankTrnRowNumber     = row^.rowNumber
                          , _bankTrnAmount        = row^.amount
                          , _bankTrnCurrency      = row^.currency
                          , _bankTrnDay           = fromMaybe saneDefaultDay (row^.day)
                          , _bankTrnAccountNumber = row^.accountNumber
                          , _bankTrnDescription   = row^.description
                          }

  saneDefaultDay :: Day
  saneDefaultDay = fromGregorian 2017 01 01

  failWithParseError :: Text -> RIO IConfig a
  failWithParseError = fail . T.unpack

  statementNewHash :: StatementHash
  statementNewHash = calcStatementHash name content

  statementNewId ::DBId BankStatement
  statementNewId = DBId statementNewHash

  statementNew :: BankStatement
  statementNew = BankStatement
    { _bankStatementId = statementNewId
    , _bankStatementSourceName = name
    , _bankStatementSourceHash = calcContentHash content
    }


getStatementTransactions :: DBId BankStatement -> RIO IConfig [BankTrn]
getStatementTransactions hash = do
  pure []

insertStatementToDb :: BankStatement -> RIO IConfig ()
insertStatementToDb stmt = do
  pure ()

insertTrnToDb :: BankTrn -> RIO IConfig ()
insertTrnToDb trn = do
  pure ()
