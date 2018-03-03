{-# LANGUAGE TemplateHaskell #-}
module Service.Bank where

import Data.Time.Calendar (Day)
import Control.Lens
import Data.Text (Text)
import Core.Bank as Bank
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.ByteString (ByteString)
import Core.SharedLens

identity :: a -> a
identity = Prelude.id

type ParseError = Text
type ParseResult = Either ParseError [ParsedBankRow]
type StatementText = ByteString

data Handle = Handle
  { _saveStatement :: SaveStatementFun
  , _getTransactions :: forall m. MonadIO m => Day -> m [BankTrn]
  }

data ParsedBankRow = ParsedBankRow
  { _parsedBankRowRowNumber :: !Int
  , _parsedBankRowAmount :: !Int
  , _parsedBankRowCurrency :: !(Maybe Text)
  , _parsedBankRowDay :: !(Maybe Day)
  , _parsedBankRowAccountNumber :: !(Maybe Text)
  , _parsedBankRowDescription :: !(Maybe Text)
  }

type StatementParserFun = (StatementText -> ParseResult)
type SaveStatementFun = forall m. MonadIO m => StatementParserFun -> StatementText -> m (Either ParseError [ParsedBankRow])

class HasBankHandle a where
  getBankHandle :: a -> Handle

instance HasBankHandle Handle where
  getBankHandle = identity

saveStatement :: (MonadIO m, HasBankHandle env, MonadReader env m) => StatementParserFun -> StatementText -> m ParseResult
saveStatement parser statement = do
  hndl <- asks getBankHandle
  _saveStatement hndl parser statement

makeFields ''ParsedBankRow
