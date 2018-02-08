{-# LANGUAGE DeriveGeneric #-}
module Parser.ABNTab where


import           GHC.Generics
import           Data.Scientific (Scientific)
import           Data.Char (ord)
import           Data.Vector
import           Data.Text (Text)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BL8
import qualified Data.Text as T
import           Data.Csv
import           Data.Time.Calendar (Day)
import Data.Time

import           Core.Bank

parse :: BL.ByteString -> Either Text [BankTrn]
parse _ = undefined

data YYYYMMDD = YYYYMMDD Day deriving (Show)
data Amnt = Amnt Int deriving (Show)

data CSVRow = CSVRow { rowId :: Text
                     , rowCurrency :: Text
                     , rowDate :: YYYYMMDD
                     , rowBalanceBefore :: Amnt
                     , rowBalanceAfter :: Amnt
                     , rowExecDate :: YYYYMMDD
                     , rowAmount :: Amnt
                     , rowDesc :: Text
                     } deriving (Show, Generic)
instance FromRecord CSVRow

instance FromField YYYYMMDD where
  parseField field = do
    parsed <- parseTimeM False defaultTimeLocale "%Y%m%d" (BL8.unpack field)
    pure $ YYYYMMDD parsed

instance FromField Amnt where
  parseField field = do
    let withoutComma = BL8.filter (/= '\t') field
    pure $ Amnt $ read $ BL8.unpack $ withoutComma

go = do
  content <- BL.readFile "/home/binarin/Yandex.Disk/TXT180208211132.TAB"
  let it :: Either String (Vector CSVRow) = decodeWith (defaultDecodeOptions { decDelimiter = fromIntegral (ord '\t') }) HasHeader content
  pure it

-- date - 3
-- amount - 7
-- description - 8
