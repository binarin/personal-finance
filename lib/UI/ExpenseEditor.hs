{-# LANGUAGE RecordWildCards #-}
module UI.ExpenseEditor
  ( module CSS
  , mkExpenseEditor
  , ExpenseEditorWidget(..)
  ) where

import           Data.Default
import           Text.Read (readMaybe)
import qualified Data.Text as T
import           Data.Scientific (scientific, Scientific)
import           Core.SharedLens
import           Service.Account as SvcAcc
import           Core.Bank as Bank
import           Core.Account as Acc

import           UICommon
import qualified UIQual as UI

import           UI.ExpenseEditorCSS as CSS

import           UI.CategorySelector

data ExpenseEditorWidget = ExpenseEditorWidget
  { _elementBE :: Element
  }

instance Widget ExpenseEditorWidget where getElement = _elementBE

wrapLabel :: String -> Element -> UI Element
wrapLabel label elt = do
  UI.label # set UI.text label
           #+ [pure elt]


data EditorState = EditorState (Maybe Scientific) (Maybe Category) (Maybe Text) deriving (Show)
instance Default EditorState where def = EditorState Nothing Nothing Nothing


mkExpenseEditor :: SvcAcc.Handle -> Event BankTrn -> UI ExpenseEditorWidget
mkExpenseEditor svcAcc trnSelectEv = do
  amountInput <- UI.input
  amountElt <- wrapLabel "Amount" amountInput

  let amountParsedE :: Event (Maybe Scientific) = readMaybe <$> UI.valueChange amountInput
  amountParsedB <- stepper Nothing amountParsedE

  cats <- liftIO $ SvcAcc.getCategories svcAcc ExpenseCategory

  let catTypeB = pure ExpenseCategory
  let catB = pure $ Nothing
  categoryWidget <- mkCategorySelector svcAcc catTypeB catB

  addButton <- UI.button #+ [UI.string "Add"]

  descInput <- UI.textarea
    # set UI.rows "5"
    # set UI.cols "100"

  let descE = Just . T.pack <$> UI.valueChange descInput
  descB <- stepper Nothing descE

  let categoryB = facts (userSelectedCategory categoryWidget)

  let editorStateB = EditorState <$> amountParsedB <*> catB <*> descB

  _elementBE <- block "expense-editor"
    [ bem "amount" amountElt
    , bem "category" categoryWidget
    , bem "description" descInput
    , bem "add" addButton
    ]
  onEvent trnSelectEv $ \trn -> do
    pure amountInput # set UI.value (formatAmount $ trn^.amount)
    pure descInput # set UI.text (T.unpack $ fromMaybe "" $ trn^.description)

  onEvent (editorStateB <@ UI.click addButton) $ \es -> do
    amountRaw <- get UI.value amountInput
    liftIO $ putStrLn $ show es
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
