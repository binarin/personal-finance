module Service.Account where

import Core.Account as Acc

data Handle = Handle { insertTransaction :: Acc.Transaction -> IO () }
