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


data Tag = Tag { _tagName :: !Text }
makeFields ''Tag

data Category = Category { _tagName :: !Text }

data Expense = Expense { _expenseAmount :: !Int
                       , _expenseAccount :: !Account
                       , _expenseCurrency :: !Text
                       , _expenseCategory :: !Category
                       , _expenseTags :: !(Seq Tag)
                       , _expenseDay :: Day
                       , _expenseDescription :: Maybe Text
                       }
makeFields ''Expense

data Transaction = TrExpense Expense
