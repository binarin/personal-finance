module Service.Account where

import Core.Account as Acc

import Data.Time.Calendar (Day)

data Handle = Handle { insertTransaction :: Acc.Transaction -> IO ()
                     , getTransactions :: Account -> Day -> IO [Transaction]
                     }
