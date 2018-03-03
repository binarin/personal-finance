{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Impl.ToshlAccount
  ( Config(..)
  , newHandle
  , url
  , token
  ) where

import           Data.Maybe (fromMaybe)
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
import           Data.Scientific (Scientific, scientific, toBoundedInteger)
import           Data.Sequence (Seq)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Calendar (Day)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           GHC.Generics
import qualified Network.Wreq as W
import           Prelude hiding (id)

import Core.SharedLens
import qualified Service.Account as Svc
import           Core.Account as Acc
import           RIO

import           Service.Log hiding (Handle)
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

newtype ToshlId name = ToshlId Text deriving (Generic, Show, Read, Eq)
instance FromJSON (ToshlId a)
instance ToJSON (ToshlId a)

serializeId :: ToshlId a -> Text
serializeId (ToshlId a) = a


data ToshlAccount = ToshlAccount
    { _toshlAccountId :: !(ToshlId ToshlAccount)
    , _toshlAccountName :: !Text
    , _toshlAccountCurrencyCode :: !Text
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

data ToshlEntryTrn = ToshlEntryTrn
  { _toshlEntryTrnId :: !(ToshlId ToshlEntryTrn)
  , _toshlEntryTrnAccount :: !(ToshlId ToshlAccount)
  , _toshlEntryTrnCurrencyCode :: !Text
  } deriving (Eq)

data ToshlEntry = ToshlEntry
    { _toshlEntryId :: !(ToshlId ToshlEntry)
    , _toshlEntryAmount :: !Scientific
    , _toshlEntryCurrencyCode :: !Text
    , _toshlEntryDate :: !Day
    , _toshlEntryDesc :: !(Maybe Text)
    , _toshlEntryAccount :: !(ToshlId ToshlAccount)
    , _toshlEntryCategory :: !(Maybe (ToshlId ToshlCategory))
    , _toshlEntryTags :: ![ToshlId ToshlTag]
    , _toshlEntryTransaction :: !(Maybe ToshlEntryTrn)
    }

makeFields ''Config
makeFields ''IConfig
makeFields ''CreateEntry
makeFields ''ToshlAccount
makeFields ''ToshlCategory
makeFields ''ToshlTag
makeFields ''ToshlEntry
makeFields ''ToshlEntryTrn

identity x = x

newHandle :: Config -> IO Svc.Handle
newHandle config = do
  accountsTVar <- atomically $ newTVar []
  categoriesTVar <- atomically $ newTVar []
  tagsTVar <- atomically $ newTVar []
  let iConfig = IConfig (config^.url) (config^.token) (config^.logger) accountsTVar categoriesTVar tagsTVar
  runRIO iConfig $ populateCaches
  pure $ Svc.Handle { Svc.insertTransaction = runRIO iConfig . insertTransaction
                    , Svc.getTransactions = \a d -> runRIO iConfig $ getTransactions a d
                    , Svc.getCategories = \ct -> runRIO iConfig $ getCategories ct
                    }

getTransactions :: Account -> Day -> RIO IConfig [Transaction]
getTransactions acc day = do
    accountId <- resolveAccount acc
    entries :: [ToshlEntry] <- getAll "/entries" [("from", [T.pack $ yyyyMmDd day])
                                                 ,("to", [T.pack $ yyyyMmDd day])
                                                 ,("accounts", [serializeId accountId])
                                                 ]
    mapM unpackTransaction entries

getCategories :: CategoryType -> RIO IConfig [Category]
getCategories ct = do
  cats <- view categories >>= liftIO . atomically . readTVar
  let toshlCats = filter (\cat -> cat^.catType == ct) cats
  pure $ map (\tc -> Category (tc^.name) (tc^.catType)) toshlCats

unpackTransaction :: ToshlEntry -> RIO IConfig Transaction
unpackTransaction tEntry
  | (tEntry^.transaction) /= Nothing = TrTransfer <$> unpackTransfer tEntry
  | (tEntry^.amount) > 0 = TrIncome <$> unpackIncome tEntry
  | otherwise = TrExpense <$> unpackExpense tEntry


unpackTransfer :: ToshlEntry -> RIO IConfig Transfer
unpackTransfer entry = do
  let _transferAmount = fromMaybe 0 $ toBoundedInteger $ 100 * (entry^.amount)
  _transferFromAccount <- findAccount (entry^.account)
  trnInfo <- case entry^.transaction of
    Nothing -> fail "unpackTransfer got entry without transaction"
    Just it -> pure it
  _transferToAccount <- findAccount (trnInfo^.account)
  let _transferFromCurrency = entry^.currencyCode
  let _transferToCurrency = trnInfo^.currencyCode
  _transferTags <- mapM findTag (entry^.tags)
  let _transferDay = entry^.date
  let _transferDescription = entry^.desc
  pure $ Transfer {..}

unpackIncome :: ToshlEntry -> RIO IConfig Income
unpackIncome entry = do
  let _incomeAmount = fromMaybe 0 $ toBoundedInteger $ 100 * (entry^.amount)
  _incomeAccount <- findAccount (entry^.account)
  let _incomeCurrency = entry^.currencyCode
  _incomeCategory <- case entry^.category of
    Nothing -> fail "unpackIncome got entry without category" -- XXX pretty dump?
    Just catId -> findCategory catId
  _incomeTags <- mapM findTag (entry^.tags)
  let _incomeDay = entry^.date
  let _incomeDescription = entry^.desc
  pure $ Income {..}

unpackExpense :: ToshlEntry -> RIO IConfig Expense
unpackExpense entry = do
    let _expenseAmount = fromMaybe 0 $ toBoundedInteger $ 100 * (entry^.amount)
    _expenseAccount <- findAccount (entry^.account)
    let _expenseCurrency = entry^.currencyCode
    _expenseCategory <- case entry^.category of
      Nothing -> fail "unpackExpense got entry without category" -- XXX pretty dump?
      Just catId -> findCategory catId
    _expenseTags <- mapM findTag (entry^.tags)
    let _expenseDay = entry^.date
    let _expenseDescription = entry^.desc
    pure $ Expense {..}

findCached :: Lens' IConfig (TVar [a]) -> (a -> Bool) -> (a -> b) -> RIO IConfig b
findCached accessor predicate transformer = do
    tvar <- view accessor
    lst <- liftIO $ atomically $ readTVar tvar
    case filter predicate lst of
      it:_ -> pure $ transformer it
      _ -> fail "No cached element"

findTag :: ToshlId ToshlTag -> RIO IConfig Tag
findTag needle = findCached tags (\it -> it^.id == needle) $ \ttag ->
    Tag $ ttag^.name

findCategory :: ToshlId ToshlCategory -> RIO IConfig Category
findCategory needle = findCached categories (\it -> it^.id == needle) $ \tcat ->
    Category (tcat^.name) (tcat^.catType)

findAccount :: ToshlId ToshlAccount -> RIO IConfig Account
findAccount needle = findCached accounts (\it -> it^.id == needle) $ \tacc ->
    Account (tacc^.name) (tacc^.currencyCode)

prefetchAll :: (Read a, Show a, FromJSON a) => Method -> Lens' IConfig (TVar [a]) -> RIO IConfig ()
prefetchAll method accessor = do
    let filename = "." <> C8.unpack method -- XXX filter non-alphanumerics?
    hasFile <- liftIO $ doesFileExist filename
    items <- case hasFile of
              True -> do
                  logInfo $ "Loading cached " <> T.decodeUtf8 method <> " from " <> T.pack filename
                  read <$> (liftIO $ readFile filename)
              False -> do
                  logInfo $ "Prefetching " <> T.decodeUtf8 method
                  items <- getAll method []
                  logInfo $ "Saving " <> T.pack (show (length items)) <> " items to cache " <> T.pack filename
                  liftIO $ withFile filename WriteMode $ \handle -> do
                      hPrint handle items
                  pure items
    logInfo $ T.decodeUtf8 method <> " has " <> T.pack (show (length items)) <> " items"
    writeTVar <$> view accessor <*> pure items >>= liftIO . atomically


populateCaches :: RIO IConfig ()
populateCaches = do
    logInfo "Populating caches"
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

instance FromJSON ToshlEntryTrn where
  parseJSON = withObject "ToshlEntryTrn" $ \v -> ToshlEntryTrn
    <$> v .: "id"
    <*> v .: "account"
    <*> ((v .: "currency") >>= (.: "code"))


instance FromJSON ToshlEntry where
    parseJSON = withObject "ToshlEntry" $ \v -> ToshlEntry
        <$> v .: "id"
        <*> v .: "amount"
        <*> ((v .: "currency") >>= (.: "code"))
        <*> v .: "date"
        <*> v .: "desc"
        <*> v .: "account"
        <*> v .:? "category"
        <*> (maybe [] identity <$> v .:? "tags")
        <*> v .:? "transaction"


type Method = ByteString
type Param = (T.Text, [T.Text])

getAll :: FromJSON a => Method -> [Param] -> RIO IConfig [a]
getAll method params = do
    baseUrl <- view url
    logInfo $ "Getting head page at " <> T.decodeUtf8 baseUrl <> " " <> T.pack (show params)
    first <- W.asJSON =<< get method params
    let pagingStr = T.decodeUtf8 $ first ^. W.responseHeader "link"
    paging <- either (fail . T.unpack) pure (parseToshlPaging pagingStr)
    rest <- case pageNext paging of
              Nothing -> pure []
              Just link -> do
                  logInfo $ "Fetching next page " <> link
                  getAll (T.encodeUtf8 link) []
    let headItems = first ^. W.responseBody
    logInfo $ T.pack (show (length headItems)) <> " items on head page, " <> T.pack (show (length rest)) <> " on remaining pages"
    pure $ headItems ++ rest

get :: Method -> [Param] -> RIO IConfig (W.Response BL.ByteString)
get method params = do
    baseUrl <- view url
    let fullUrl = B.concat [baseUrl, method]
    ourToken <- view token
    logInfo $ "GET " <> T.decodeUtf8 fullUrl <> T.pack (show params)
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


yyyyMmDd :: Day -> String
yyyyMmDd = formatTime defaultTimeLocale "%Y-%m-%d"
