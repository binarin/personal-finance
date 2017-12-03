{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Impl.ToshlAccount
  ( Config(..)
  , newHandle
  , url
  , token
  ) where

import           Data.Monoid
import           Prelude hiding (id)
import           GHC.Generics
import           Data.Aeson as J
import qualified Data.ByteString.Char8 as C8
import           Control.Monad.IO.Class
import Control.Monad (void)
import           Control.Lens
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Scientific (Scientific, scientific)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Calendar (Day)
import qualified Network.Wreq as W
import Data.Time.Format (formatTime, defaultTimeLocale)

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

newtype ToshlId name = ToshlId Text deriving (Generic)
instance FromJSON (ToshlId a)
instance ToJSON (ToshlId a)

data ToshlAccount = ToshlAccount
    { _toshlAccountId :: !(ToshlId ToshlAccount)
    , _toshlAccountName :: !Text
    , _toshlAccountcurrencyCode :: !Text
    }

data ToshlCategory = ToshlCategory
    { _toshlCategoryId :: !(ToshlId ToshlCategory)
    , _toshlCategoryName :: !Text
    , _toshlCategoryCatType :: !CategoryType
    }

data CreateEntry = CreateEntry
    { _createEntryAmount :: !Scientific
    , _createEntryCurrencyCode :: !Text
    , _createEntryDate :: !Day
    , _createEntryAccount :: !(ToshlId ToshlAccount)
    , _createEntryCategory :: !(ToshlId ToshlCategory)
    }

makeFields ''CreateEntry
makeFields ''ToshlAccount
makeFields ''ToshlCategory

newHandle :: Config -> Svc.Handle
newHandle config = Svc.Handle { Svc.insertTransaction = runRIO iConfig . insertTransaction }
  where
    iConfig = IConfig (config^.url) (config^.token)

insertTransaction :: Acc.Transaction -> RIO IConfig ()
insertTransaction (TrExpense expense) = do
    reqBody <- serializeCreate expense
    liftIO $ putStrLn $ show $ (encode reqBody)
    void $ post "/entries" [] reqBody

formatAmount :: Int -> Scientific
formatAmount cents = scientific (fromIntegral cents) (-2)

resolveAccount :: Account -> RIO IConfig (ToshlId ToshlAccount)
resolveAccount acc = do
    resp :: W.Response [ToshlAccount] <- W.asJSON =<< get "/accounts" []
    case filter (\tAcc -> tAcc ^.name == acc ^. name) (resp ^. W.responseBody) of
      [] -> fail $ "No matching account" ++ T.unpack (acc^.name)
      it:_ -> return $ it ^. id

resolveCategory :: Category -> RIO IConfig (ToshlId ToshlCategory)
resolveCategory cat = do
    catTypeStr <- case cat^.catType of
                     ExpenseCategory -> pure "expense"
                     IncomeCategory -> pure "income"
                     _ -> fail "can only resolve income and expense categories"
    resp :: W.Response [ToshlCategory] <- W.asJSON =<< get "/categories" [("type", [catTypeStr])]
    case filter (\tCat -> tCat^.name == cat^.name) (resp^.W.responseBody) of
      [] -> fail $ "No matching category: " <> T.unpack (cat^.name)
      it:_ -> return $ it ^. id

serializeCreate :: Acc.Expense -> RIO IConfig CreateEntry
serializeCreate expense = do
    accountId <- resolveAccount (expense ^. account)
    categoryId <- resolveCategory (expense ^. category)
    return $ CreateEntry (formatAmount (negate $ expense^.amount)) (expense ^. currency) (expense ^. day) accountId categoryId

instance FromJSON ToshlAccount where
    parseJSON = withObject "ToshlAccount" $ \v -> ToshlAccount
      <$> v .: "id"
      <*> v .: "name"
      <*> ((v .: "currency") >>= (.: "code"))

instance FromJSON ToshlCategory where
    parseJSON = withObject "ToshlCategory" $ \v -> ToshlCategory
      <$> v .: "id"
      <*> v .: "name"
      <*> ((v .: "type") >>= withText "categoryType" (\case
                                                        "income" -> pure IncomeCategory
                                                        "expense" -> pure ExpenseCategory
                                                        "system" -> pure SystemCategory
                                                        cat -> fail $ "bad category: " <> T.unpack cat))

type Method = ByteString
type Param = (T.Text, [T.Text])

get :: Method -> [Param] -> RIO IConfig (W.Response BL.ByteString)
get method params = do
    baseUrl <- view url
    let fullUrl = B.concat [baseUrl, method]
    ourToken <- view token
    liftIO $ W.getWith (W.defaults & addAuth ourToken & addParams params) (C8.unpack fullUrl)

post :: ToJSON a => Method -> [Param] -> a -> RIO IConfig (W.Response BL.ByteString)
post method params body = do
    io <- W.postWith <$> mkOptions params <*> mkUrl method <*> pure (encode body)
    resp <- liftIO io
    return resp

mkUrl :: Method -> RIO IConfig String
mkUrl method = do
    baseUrl <- view url
    pure $ C8.unpack $B.concat [baseUrl, method]

mkOptions :: [Param] -> RIO IConfig W.Options
mkOptions params = do
  ourToken <- view token
  return $ W.defaults & addAuth ourToken & addParams params

addAuth :: ByteString -> W.Options -> W.Options
addAuth token = W.auth ?~ W.basicAuth token ""

addParams :: [Param] -> W.Options -> W.Options
addParams [] opts = opts
addParams ((n, vs):ps) opts =
    opts & W.param n .~ vs & addParams ps

instance ToJSON CreateEntry where
    toJSON ce = object [ "amount" J..= (ce^.amount)
                       , "currency" J..= currencyObj
                       , "date" J..= yyyy_mm_dd
                       , "account" J..= (ce^.account)
                       , "category" J..= (ce^.category)
                       ]
      where
        currencyObj = object [ "code" J..= (ce^.currencyCode)]
        yyyy_mm_dd = formatTime defaultTimeLocale "%Y-%m-%d" (ce^.date)
