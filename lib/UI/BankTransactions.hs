{-# LANGUAGE RecordWildCards #-}
module UI.BankTransactions
  ( module CSS
  , mkBankEntries
  , BankEntriesWidget(..)
  ) where

import qualified Data.Text as T

import           UICommon
import qualified UIQual as UI

import           Core.Bank
import qualified Service.Bank as SvcBank
import qualified Service.Log as SvcLog

import UI.BankTransactionsCSS as CSS

data BankEntriesWidget = BankEntriesWidget
  { _elementBE :: Element
  , _selectedBE :: Event BankTrn
  }

instance Widget BankEntriesWidget where getElement = _elementBE


mkBankEntries :: SvcBank.Handle -> SvcLog.Handle -> Tidings Day -> UI BankEntriesWidget
mkBankEntries bankSvc logSvc dayT = do
  initialDay <- liftIO $ currentValue (facts dayT)
  trns <- SvcBank._getTransactions bankSvc initialDay
  (modifyTrnsEv, modifyTrns) <- liftIO $ newEvent
  (_selectedBE, selectTrn) <- liftIO $ newEvent
  trnsB <- liftIO $ accumB trns modifyTrnsEv
  void $ onEvent (rumors dayT) $ \newDay -> do
    newTrns <- liftIO $ sortTransactions <$> SvcBank._getTransactions bankSvc newDay
    liftIO $ modifyTrns $ const newTrns
    pure ()
  _elementBE <- UI.div
  element _elementBE # sink childrenM (map <$> pure (showTransaction selectTrn) <*> trnsB)
  pure BankEntriesWidget {..}


showTransaction :: UICommon.Handler BankTrn -> BankTrn -> UI Element
showTransaction selectAction trn = do
  amountElt <- makeAmountElement (trn^.amount) (fromMaybe "EUR" $ trn^.currency)
  dateElt <- divWithDate (trn^.day)
  descElt <- UI.string (T.unpack $ fromMaybe "" $ trn^.description)
  container <- block "bank-transaction" [("amount", amountElt)
                                        ,("date", dateElt)
                                        ,("description", descElt)
                                        ]
  on UI.click container $ \_ -> do
    liftIO $ selectAction trn
  pure container
