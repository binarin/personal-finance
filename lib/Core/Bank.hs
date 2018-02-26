{-# LANGUAGE TemplateHaskell #-}
module Core.Bank where

import Data.List (sortBy)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Control.Lens
import Data.ByteString (ByteString)

newtype DBId a = DBId Text deriving (Eq, Ord, Show)

type StatementName = Text
type StatementHash = Text
type StatementContent = ByteString

data BankStatement = BankStatement
  { _bankStatementId :: !(DBId BankStatement)
  , _bankStatementSourceName :: !StatementName
  , _bankStatementSourceHash :: !StatementHash
  }

data BankTrn = BankTrn
  { _bankTrnId :: !(DBId BankTrn)
--  , _bankTrnStatement :: !BankStatement
--  , _bankTrnRowNumber :: !Int
  , _bankTrnAmount :: !Int
  , _bankTrnCurrency :: !(Maybe Text)
  , _bankTrnDay :: !Day
  , _bankTrnAccountNumber :: !(Maybe Text)
  , _bankTrnDescription :: !(Maybe Text)
  } deriving (Show)

makeFields ''BankStatement
makeFields ''BankTrn


sortTransactions :: (HasAmount s a, Ord a) => [s] -> [s]
sortTransactions = sortBy (\x y -> (x^.amount) `compare` (y^.amount))
