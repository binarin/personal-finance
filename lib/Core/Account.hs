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

data Transaction = TrExpense Expense deriving (Show, Eq, Ord)

makeFields ''Expense
makeFields ''Category
makeFields ''Tag
