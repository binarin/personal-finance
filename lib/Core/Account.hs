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
                       }
makeFields ''Account

data CategoryType = ExpenseCategory | IncomeCategory | SystemCategory deriving (Eq, Show)

data Tag = Tag { _tagName :: !Text }

data Category = Category { _categoryName :: !Text
                         , _categoryCatType :: !CategoryType
                         }

data Expense = Expense { _expenseAmount :: !Int
                       , _expenseAccount :: !Account
                       , _expenseCurrency :: !Text
                       , _expenseCategory :: !Category
                       , _expenseTags :: ![Tag]
                       , _expenseDay :: Day
                       , _expenseDescription :: Maybe Text
                       }

data Transaction = TrExpense Expense

makeFields ''Expense
makeFields ''Category
makeFields ''Tag
