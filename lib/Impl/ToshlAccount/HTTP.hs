{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Impl.ToshlAccount.HTTP
  ( parseToshlPaging
  , ToshlPaging(..)
  ) where

import           Data.Monoid
import           Control.Monad (void)
import           Control.Arrow (left)
import           Text.Parsec
import           Text.Parsec.Text
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Default

data ToshlPaging = ToshlPaging { pageFirst :: !(Maybe Text)
                               , pageLast :: !(Maybe Text)
                               , pageNext :: !(Maybe Text)
                               , pagePrevious :: !(Maybe Text)
                               } deriving (Eq, Show)

instance Default ToshlPaging where
    def = ToshlPaging Nothing Nothing Nothing Nothing

-- <link>; rel="rel"
parseToshlPaging :: Text -> Either Text ToshlPaging
parseToshlPaging str = left showAsText parsed
  where
    parsed = runParser parts def "(unknown)" str

    parts :: Parsec Text ToshlPaging ToshlPaging
    parts = do
        many (chunk >> (string_ ", " <|> eof))
        eof
        getState

    string_ a = void $ string a

    manyT p = Just . T.pack <$> many p

    chunk = do
        string_ "<"
        path <- manyT (noneOf ">")
        string_ ">; rel=\""
        kind <- many (noneOf "\"")
        string_ "\""
        case kind of
          "first" -> modifyState (\u -> u { pageFirst = path})
          "next" -> modifyState (\u -> u { pageNext = path})
          "previous" -> modifyState (\u -> u { pagePrevious = path })
          "last" -> modifyState (\u -> u { pageLast = path})
          _ -> unexpected $ "kind " <> kind

    showAsText = T.pack . show
