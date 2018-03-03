{-# LANGUAGE RecordWildCards #-}
module UI.ExpenseEditor
  ( module CSS
  , mkExpenseEditor
  , ExpenseEditorWidget(..)
  ) where

import qualified Data.Text as T
import Data.Scientific (scientific)
import Core.SharedLens
import Service.Account as SvcAcc
import Core.Bank as Bank
import Core.Account as Acc

import           UICommon
import qualified UIQual as UI

import UI.ExpenseEditorCSS as CSS

import UI.CategorySelector

data ExpenseEditorWidget = ExpenseEditorWidget
  { _elementBE :: Element
  }

instance Widget ExpenseEditorWidget where getElement = _elementBE

wrapLabel :: String -> Element -> UI Element
wrapLabel label elt = do
  UI.label # set UI.text label
           #+ [pure elt]

mkExpenseEditor :: SvcAcc.Handle -> Event BankTrn -> UI ExpenseEditorWidget
mkExpenseEditor svcAcc trnSelectEv = do
  amountInput <- UI.input
  amountElt <- wrapLabel "Amount" amountInput

  cats <- liftIO $ SvcAcc.getCategories svcAcc ExpenseCategory

  let catTypeB = pure ExpenseCategory
  let catB = pure $ Nothing
  categoryWidget <- mkCategorySelector svcAcc catTypeB catB

  addButton <- UI.button #+ [UI.string "Add"]

  _elementBE <- block "expense-editor"
    [ bem "amount" amountElt
    , bem "category" categoryWidget
    , bem "add" addButton
    ]
  onEvent trnSelectEv $ \trn -> do
    pure amountInput # set UI.value (formatAmount $ trn^.amount)
  on UI.click addButton $ \_ -> do
    liftIO $ putStrLn "clicked"
  -- amount
  -- account
  -- currency
  -- category
  -- tags
  -- day
  -- description
  pure ExpenseEditorWidget{..}

formatAmount :: Int -> String
formatAmount amt = show $ scientific (fromIntegral $ abs amt) (-2)
