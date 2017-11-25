{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Core.Account where

import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Sequence (Seq)

data Account = Account { name :: !Text
                       , currency :: !Text
                       }

data Tag = Tag { name :: !Text }

data Category = Category { name :: !Text }

data Expense = Expense { amount :: !Int
                       , account :: !Account
                       , currency :: !Text
                       , category :: !Category
                       , tags :: !(Seq Tag)
                       , day :: Day
                       , description :: Maybe Text
                       }

data Transaction = TrExpense Expense
