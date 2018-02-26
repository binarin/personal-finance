{-# LANGUAGE RecordWildCards #-}
module UI.ExpenseEditor
  ( module CSS
  , mkExpenseEditor
  , ExpenseEditorWidget(..)
  ) where

import qualified Data.Text as T

import Core.Bank
import           UICommon
import qualified UIQual as UI

import UI.ExpenseEditorCSS as CSS

data ExpenseEditorWidget = ExpenseEditorWidget
  { _elementBE :: Element
  }

instance Widget ExpenseEditorWidget where getElement = _elementBE

wrapLabel :: String -> Element -> UI Element
wrapLabel label elt = do
  UI.label # set UI.text label
           #+ [pure elt]

mkExpenseEditor :: Event BankTrn -> UI ExpenseEditorWidget
mkExpenseEditor trnSelectEv = do
  amountInput <- UI.input
  amountElt <- wrapLabel "Amount" amountInput
  addButton <- UI.button #+ [UI.string "Add"]
  _elementBE <- block "expense-editor"
    [("amount", amountElt)
    ,("add", addButton)
    ]
  onEvent trnSelectEv $ \trn -> do
    pure amountInput # set UI.value (show $ trn^.amount)
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
