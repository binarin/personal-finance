{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Core.Account where

import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Sequence (Seq)
import Control.Lens

data Account = Account { _accountName :: !Text
                       , _accountCurrency :: !Text
                       } deriving (Show, Eq, Ord)
makeFields ''Account

data CategoryType = ExpenseCategory | IncomeCategory | SystemCategory deriving (Eq, Ord, Show, Read)

data Tag = Tag { _tagName :: !Text } deriving (Show, Eq, Ord)

data Category = Category { _categoryName :: !Text
                         , _categoryCatType :: !CategoryType
                         } deriving (Show, Eq, Ord)

data Expense = Expense { _expenseAmount :: !Int
                       , _expenseAccount :: !Account
                       , _expenseCurrency :: !Text
                       , _expenseCategory :: !Category
                       , _expenseTags :: ![Tag]
                       , _expenseDay :: Day
                       , _expenseDescription :: Maybe Text
                       } deriving (Show, Eq, Ord)

data Income = Income { _incomeAmount :: !Int
                     , _incomeAccount :: !Account
                     , _incomeCurrency :: !Text
                     , _incomeCategory :: !Category
                     , _incomeTags :: ![Tag]
                     , _incomeDay :: Day
                     , _incomeDescription :: Maybe Text
                     } deriving (Show, Eq, Ord)


data Transfer = Transfer { _transferAmount :: !Int
                         , _transferFromAccount :: !Account
                         , _transferToAccount :: !Account
                         , _transferFromCurrency :: !Text
                         , _transferToCurrency :: !Text
                         , _transferTags :: ![Tag]
                         , _transferDay :: Day
                         , _transferDescription :: Maybe Text
                         } deriving (Show, Eq, Ord)





data Transaction = TrExpense Expense
                 | TrIncome Income
                 | TrTransfer Transfer
  deriving (Show, Eq, Ord)


makeFields ''Expense
makeFields ''Income
makeFields ''Transfer
makeFields ''Category
makeFields ''Tag
