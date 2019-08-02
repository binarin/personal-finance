{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Config
  ( PartialConfig (..)
  , Config (..)
  , makeConfig
  , toshlUrl
  , toshlToken
  ) where

import Data.ByteString (ByteString)
import Data.Monoid (Last(..), (<>))
import Data.Semigroup ()
import Data.Default
import Control.Lens

data PartialConfig = PartialConfig { _partialConfigToshlUrl :: Last ByteString
                                   , _partialConfigToshlToken :: Last ByteString
                                   } deriving (Eq, Show)
makeFields ''PartialConfig


data Config = Config { _configToshlUrl :: ByteString
                     , _configToshlToken :: ByteString
                     }
makeFields ''Config

instance Default PartialConfig where
  def = PartialConfig mempty mempty

instance Semigroup PartialConfig where
    (<>) :: PartialConfig -> PartialConfig -> PartialConfig
    (<>) x y = def & toshlUrl .~ (x^.toshlUrl) <> (y^.toshlUrl)
                   & toshlToken .~ (x^.toshlToken) <> (y^.toshlToken)


instance Monoid PartialConfig where
    mempty = PartialConfig mempty mempty
    mappend = (<>)

lastToEither :: String -> Last a -> Either String a
lastToEither errMsg (Last x) = maybe (Left errMsg) Right x


makeConfig :: PartialConfig -> Either String Config
makeConfig pc = do
    Config <$> lastToEither "missing toshl url" (pc^.toshlUrl)
           <*> lastToEither "missing toshl token" (pc^.toshlToken)
