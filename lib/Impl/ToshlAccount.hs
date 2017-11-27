{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Impl.ToshlAccount
  ( Config(..)
  , newHandle
  , url
  , token
  ) where

import           Control.Lens
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Calendar (Day)
import qualified Network.Wreq as W

import qualified Service.Account as Svc
import           Core.Account as Acc
import           RIO

data Config = Config { _configUrl :: ByteString
                     , _configToken :: ByteString
                     }
makeFields ''Config

data IConfig = IConfig { _iConfigUrl :: ByteString
                       , _iConfigToken :: ByteString
                       }
makeFields ''IConfig

newHandle :: Config -> Svc.Handle
newHandle config = Svc.Handle { Svc.insertTransaction = insertTransaction config }

insertTransaction :: Config -> Acc.Transaction -> IO ()
insertTransaction _ _ = do
    putStrLn "not implemented!"

formatAmount :: Int -> Scientific
formatAmount = undefined

resolveAccount :: Account -> RIO IConfig Text
resolveAccount = undefined

resolveCategory :: Category -> RIO IConfig Text
resolveCategory = undefined

serializeCreate :: Acc.Expense -> RIO IConfig CreateEntry
serializeCreate expense = do
    accountId <- resolveAccount (expense ^. account)
    categoryId <- resolveCategory (expense ^. category)
    return $ CreateEntry (formatAmount (expense ^. amount)) (expense ^. currency) (expense ^. day) accountId categoryId

data CreateEntry = CreateEntry
    { _createEntryAmount :: !Scientific
    , _createEntryCurrencyCode :: !Text
    , _createEntryDate :: !Day
    , _createEntryAccount :: !Text
    , _createEntryCategory :: !Text
    }

data ToshlAccount = ToshlAccount
    { _toshlAccountId :: !Text
    , _toshlAccountName :: !Text
    , _toshlAccountcurrencyCode :: !Text
    }

type Method = T.Text
type Param = (T.Text, [T.Text])

get :: Method -> [Param] -> RIO IConfig (W.Response BL.ByteString)
get = undefined

addParams :: [Param] -> W.Options -> W.Options
addParams [] opts = opts
addParams ((n, vs):ps) opts =
    opts & W.param n .~ vs & addParams ps
