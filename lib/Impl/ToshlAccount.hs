module Impl.ToshlAccount
  ( Config(..)
  , newHandle
  ) where

import Data.ByteString (ByteString)

import qualified Service.Account as Svc
import Core.Account as Acc

data Config = Config { url :: ByteString
                     , token :: ByteString
                     }

newHandle :: Config -> Svc.Handle
newHandle config = Svc.Handle { Svc.insertTransaction = insertTransaction config }

insertTransaction :: Config -> Acc.Transaction -> IO ()
insertTransaction _ _ = do
    putStrLn "not implemented!"
