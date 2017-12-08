{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Impl.ToshlAccount
  ( Config(..)
  , newHandle
  , url
  , token
  ) where

import           System.IO (IOMode(WriteMode), withFile, hPrint, hGetContents, readFile)
import           System.Directory (doesFileExist)
import           Control.Monad.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, newTVar, writeTVar, readTVar)
import           Control.Lens
import           Control.Monad (void, when)
import           Control.Monad.IO.Class
import           Data.Aeson as J
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import           Data.Monoid hiding (getAll)
import           Data.Scientific (Scientific, scientific)
import           Data.Sequence (Seq)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Calendar (Day)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           GHC.Generics
import qualified Network.Wreq as W
import           Prelude hiding (id)

import qualified Service.Account as Svc
import           Core.Account as Acc
import           RIO

import           Service.Log (HasLogHandle, lInfo)
import qualified Service.Log as Log


import           Impl.ToshlAccount.HTTP

data Config = Config { _configUrl :: ByteString
                     , _configToken :: ByteString
                     , _configLogger :: Log.Handle
                     }
data IConfig = IConfig { _iConfigUrl :: ByteString
                       , _iConfigToken :: ByteString
                       , _iConfigLogger :: Log.Handle
                       , _iConfigAccounts :: TVar [ToshlAccount]
                       , _iConfigCategories :: TVar [ToshlCategory]
                       , _iConfigTags :: TVar [ToshlTag]
                       }

instance HasLogHandle IConfig where
    getLogHandle = _iConfigLogger

newtype ToshlId name = ToshlId Text deriving (Generic, Show, Read)
instance FromJSON (ToshlId a)
instance ToJSON (ToshlId a)

data ToshlAccount = ToshlAccount
    { _toshlAccountId :: !(ToshlId ToshlAccount)
    , _toshlAccountName :: !Text
    , _toshlAccountcurrencyCode :: !Text
    } deriving (Show, Read)

data ToshlCategory = ToshlCategory
    { _toshlCategoryId :: !(ToshlId ToshlCategory)
    , _toshlCategoryName :: !Text
    , _toshlCategoryCatType :: !CategoryType
    } deriving (Show, Read)

data ToshlTag = ToshlTag
    { _toshlTagId :: !(ToshlId ToshlTag)
    , _toshlTagName :: !Text
    , _toshlTagTagType :: !CategoryType
    , _toshlTagPrimaryCategoryId :: !(ToshlId ToshlCategory)
    } deriving (Show, Read)

data CreateEntry = CreateEntry
    { _createEntryAmount :: !Scientific
    , _createEntryCurrencyCode :: !Text
    , _createEntryDate :: !Day
    , _createEntryAccount :: !(ToshlId ToshlAccount)
    , _createEntryCategory :: !(ToshlId ToshlCategory)
    , _createEntryTags :: ![ToshlId ToshlTag]
    }


makeFields ''Config
makeFields ''IConfig
makeFields ''CreateEntry
makeFields ''ToshlAccount
makeFields ''ToshlCategory
makeFields ''ToshlTag

newHandle :: Config -> IO Svc.Handle
newHandle config = do
  accountsTVar <- atomically $ newTVar []
  categoriesTVar <- atomically $ newTVar []
  tagsTVar <- atomically $ newTVar []
  let iConfig = IConfig (config^.url) (config^.token) (config^.logger) accountsTVar categoriesTVar tagsTVar
  runRIO iConfig $ populateCaches
  pure $ Svc.Handle { Svc.insertTransaction = runRIO iConfig . insertTransaction }

prefetchAll :: (Read a, Show a, FromJSON a) => Method -> Lens' IConfig (TVar [a]) -> RIO IConfig ()
prefetchAll method accessor = do
    let filename = "." <> C8.unpack method -- XXX filter non-alphanumerics?
    hasFile <- liftIO $ doesFileExist filename
    items <- case hasFile of
              True -> do
                  lInfo $ "Loading cached " <> C8.unpack method <> " from " <> filename
                  read <$> (liftIO $ readFile filename)
              False -> do
                  lInfo $ "Prefetching " <> method
                  items <- getAll method []
                  lInfo $ "Saving " <> show (length items) <> " items to cache " <> filename
                  liftIO $ withFile filename WriteMode $ \handle -> do
                      hPrint handle items
                  pure items
    lInfo $ C8.unpack method <> " has " <> show (length items) <> " items"
    writeTVar <$> view accessor <*> pure items >>= liftIO . atomically


populateCaches :: RIO IConfig ()
populateCaches = do
    lInfo $ ("Populating caches" :: Text)
    prefetchAll "/accounts" accounts
    prefetchAll "/categories" categories
    prefetchAll "/tags" tags
    pure ()

insertTransaction :: Acc.Transaction -> RIO IConfig ()
insertTransaction (TrExpense expense) = do
    reqBody <- serializeCreate expense
    liftIO $ putStrLn $ show $ (encode reqBody)
    void $ post "/entries" [] reqBody

formatAmount :: Int -> Scientific
formatAmount cents = scientific (fromIntegral cents) (-2)

resolveAccount :: Account -> RIO IConfig (ToshlId ToshlAccount)
resolveAccount acc = do
    accList <- view accounts >>= liftIO . atomically . readTVar
    case filter (\tAcc -> tAcc ^.name == acc ^. name) accList of
      [] -> fail $ "No matching account" ++ T.unpack (acc^.name)
      it:_ -> return $ it ^. id

resolveCategory :: Category -> RIO IConfig (ToshlId ToshlCategory)
resolveCategory cat = do
    catTvar <- view categories
    catList <- liftIO $ atomically $ readTVar catTvar
    case filter (\tCat -> (tCat^.name == cat^.name) && (tCat^.catType == cat^.catType)) catList of
      [] -> fail $ "No matching category: " <> T.unpack (cat^.name)
      it:_ -> return $ it^.id

resolveTags :: [Tag] -> RIO IConfig [ToshlId ToshlTag]
resolveTags [] = pure []
resolveTags (t:ts) = (:) <$> resolve (t^.name) <*> resolveTags ts
  where
    resolve thisName = do
        tagList <- view tags >>= liftIO . atomically . readTVar
        case filter (\tag -> tag^.name == thisName) tagList of
            [] -> fail $ "No tag " <> T.unpack thisName
            it:_ -> pure (it^.id)

serializeCreate :: Acc.Expense -> RIO IConfig CreateEntry
serializeCreate expense = do
    accountId <- resolveAccount (expense ^. account)
    categoryId <- resolveCategory (expense ^. category)
    tagIds <- resolveTags (expense ^. tags)
    return $ CreateEntry (formatAmount (negate $ expense^.amount)) (expense ^. currency) (expense ^. day) accountId categoryId tagIds

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


-- parseCategoryType :: Value -> Parser CategoryType
parseCategoryType = withText "categoryType" (\case
                                               "income" -> pure IncomeCategory
                                               "expense" -> pure ExpenseCategory
                                               "system" -> pure SystemCategory
                                               cat -> fail $ "bad category: " <> T.unpack cat)

instance FromJSON ToshlTag where
    parseJSON = withObject "ToshlTag" $ \v -> ToshlTag
      <$> v .: "id"
      <*> v .: "name"
      <*> (v .: "type" >>= parseCategoryType)
      <*> v .: "category"


type Method = ByteString
type Param = (T.Text, [T.Text])

getAll :: FromJSON a => Method -> [Param] -> RIO IConfig [a]
getAll method params = do
    baseUrl <- view url
    lInfo $ "Getting head page at " <> baseUrl <> " " <> T.encodeUtf8 (T.pack $ show params)
    first <- W.asJSON =<< get method params
    let pagingStr = T.decodeUtf8 $ first ^. W.responseHeader "link"
    paging <- either (fail . T.unpack) pure (parseToshlPaging pagingStr)
    liftIO $ putStrLn $ show paging
    rest <- case pageNext paging of
              Nothing -> pure []
              Just link -> do
                  lInfo $ "Fetching next page " <> link
                  getAll (T.encodeUtf8 link) []
    let headItems = first ^. W.responseBody
    lInfo $ show (length headItems) <> " items on head page, " <> show (length rest) <> " on remaining pages"
    pure $ headItems ++ rest

get :: Method -> [Param] -> RIO IConfig (W.Response BL.ByteString)
get method params = do
    baseUrl <- view url
    let fullUrl = B.concat [baseUrl, method]
    ourToken <- view token
    lInfo fullUrl
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
                       , "tags" J..= (ce^.tags)
                       ]
      where
        currencyObj = object [ "code" J..= (ce^.currencyCode)]
        yyyy_mm_dd = formatTime defaultTimeLocale "%Y-%m-%d" (ce^.date)
