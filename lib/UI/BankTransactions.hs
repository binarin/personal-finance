module UI.BankTransactions
  ( module CSS
  , mkBankEntriesElement
  ) where

import           UICommon
import qualified UIQual as UI

import           Core.Bank
import qualified Service.Bank as SvcBank
import qualified Service.Log as SvcLog

import UI.BankTransactionsCSS as CSS

mkBankEntriesElement :: SvcBank.Handle -> SvcLog.Handle -> Tidings Day -> UI Element
mkBankEntriesElement bankSvc logSvc dayT = do
  initialDay <- liftIO $ currentValue (facts dayT)
  trns <- SvcBank._getTransactions bankSvc initialDay
  (modifyTrnsEv, modifyTrns) <- liftIO $ newEvent
  trnsB <- liftIO $ accumB trns modifyTrnsEv
  void $ onEvent (rumors dayT) $ \newDay -> do
    newTrns <- liftIO $ SvcBank._getTransactions bankSvc newDay
    liftIO $ modifyTrns $ const newTrns
    pure ()
  container <- UI.div
  element container # sink childrenM (map <$> pure showTransaction <*> trnsB)

showTransaction :: BankTrn -> UI Element
showTransaction _ = UI.string "transaction!"
