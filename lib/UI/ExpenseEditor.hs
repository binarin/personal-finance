{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
module UI.ExpenseEditor
  ( module CSS
  , mkExpenseEditor
  , ExpenseEditorWidget(..)
  , transactionsChanged
  ) where

import           Text.Regex.Posix hiding (match)
import Control.Lens (Lens', (.~), (&), view)
import           Data.Default
import           Text.Read (readMaybe)
import qualified Data.Text as T
import           Data.Scientific (scientific, Scientific, toBoundedInteger)
import           Core.SharedLens
import           Service.Account as SvcAcc
import           Core.Bank as Bank
import           Core.Account as Acc

import Common
import           UICommon
import qualified UIQual as UI

import           UI.ExpenseEditorCSS as CSS

import           UI.CategorySelector

data ExpenseEditorWidget = ExpenseEditorWidget
  { _elementBE :: Element
  , _trnAddedBE :: Event ()
  }

instance Widget ExpenseEditorWidget where getElement = _elementBE

wrapLabel :: String -> Element -> UI Element
wrapLabel label elt = do
  UI.label # set UI.text label
           #+ [pure elt]


data EditorState = EditorState (Maybe Scientific) (Maybe Category) (Maybe Text) (Maybe Day) deriving (Show)
instance Default EditorState where def = EditorState Nothing Nothing Nothing Nothing

scientificAmount :: HasAmount s Int => Lens' s Scientific
scientificAmount k obj = fmap (\newScAmount -> obj & amount .~ (fromScientific newScAmount) ) (k (toScientific $ obj^.amount))
  where
    toScientific int = scientific (fromIntegral $ abs int) (-2)
    fromScientific :: Scientific -> Int
    fromScientific sc = round $ sc * 100

match :: Text -> String -> Bool
match txt re = T.unpack txt =~ re

guessCategory :: BankTrn -> Maybe Category
guessCategory BankTrn {_bankTrnDescription = Just desc}
  | desc `match` "Maandpremie.*van verzekering" = Just $ Category "Услуги" ExpenseCategory
  | desc `match` "EasyPayExtra" = c "Услуги"
  | desc `match` "Automatisch opladen OV-chipkaart" = c "Транспорт"
  | desc `match` "ALBERT HEIJN" = c "Еда и напитки"
  | desc `match` "Albert Heijn" = c "Еда и напитки"
  | desc `match` "Malinka Market" = c "Еда и напитки"
  | desc `match` "Bessaha" = c "Еда и напитки"
  | desc `match` "Aldi" = c "Еда и напитки"
  | desc `match` "CBRE DRES Custodian" = c "Дом"
  | desc `match` "CZ Groep Zorgverzekeraar" = c "Insurance"
  | desc `match` "Vomar" = c "Еда и напитки"
  | desc `match` "Gamma" = c "Дом"
  | desc `match` "KPN" = c "Связь"
  | desc `match` "Lidl" = c "Еда и напитки"
  | desc `match` "HEMA" = c "Дом"
  | desc `match` "Booking.com Bloomhouse" = c "Еда и напитки"
  | desc `match` "WWW.KPN.COM" = c "Связь"
  | otherwise = Nothing
  where
    c name = Just $ Category name ExpenseCategory
guessCategory _ = Nothing


mkExpenseEditor :: SvcAcc.Handle -> Event BankTrn -> UI ExpenseEditorWidget
mkExpenseEditor svcAcc trnSelectEv = do
  amountInput <- UI.input
  let amountPresetE :: Event (Maybe Scientific) = Just . view scientificAmount <$> trnSelectEv
  let amountInputE :: Event (Maybe Scientific) = readMaybe <$> UI.valueChange amountInput
  amountB <- stepper Nothing (unionWith const amountInputE amountPresetE)

  let catTypeB = pure ExpenseCategory

  (categoryWidget, categoryB) <- mdo
    categoryWidget <- mkCategorySelector svcAcc catTypeB categoryB
    let resetE = guessCategory <$> trnSelectEv
        userE = (rumors $ userSelectedCategory categoryWidget)
    categoryB <- stepper Nothing (unionWith const resetE userE)
    pure (categoryWidget, categoryB)

  descInput <- UI.textarea
    # set UI.rows "5"
    # set UI.cols "100"
  let descInputE = Just . T.pack <$> UI.valueChange descInput
      descPresetE = view description <$> trnSelectEv
  descB <- stepper Nothing (unionWith const descInputE descPresetE)
  element descInput # sink UI.value (T.unpack . fromMaybe "" <$> descB)

  dateInput <- UI.input
    # set UI.type_ "date"
  let datePresetE = Just . view day <$> trnSelectEv
  dateB <- stepper Nothing datePresetE
  let renderDate Nothing = ""
      renderDate (Just dt) = yyyy_mm_dd dt
  _ <- pure dateInput # sink UI.value (renderDate <$> dateB)

  amountElt <- wrapLabel "Amount: " amountInput

  addButton <- UI.button #+ [UI.string "Add"]

  let editorStateB = EditorState <$> amountB <*> categoryB <*> descB <*> dateB
  es <- UI.string "(none)" # sink UI.text (show <$> editorStateB)

  dateElt <- wrapLabel "Date: " dateInput

  _elementBE <- block "expense-editor"
    [ bem "date" dateElt
    , bem "amount" amountElt
    , bem "category" categoryWidget
    , bem "description" descInput
    , bem "add" addButton
    , bem "debug" es
    ]
  onEvent trnSelectEv $ \trn -> do
    pure amountInput # set UI.value (formatAmount $ trn^.amount)
    pure descInput # set UI.text (T.unpack $ fromMaybe "" $ trn^.description)

  (_trnAddedBE, trnAddedFire) <- liftIO $ newEvent
  onEvent (editorStateB <@ UI.click addButton) $ \es -> do
    case es of
      EditorState (Just sum) (Just cat) desc (Just dt) -> do
        liftIO $ SvcAcc.insertTransaction svcAcc $ TrExpense $ Expense
          { _expenseAmount = (round $ (sum) * 100)
          , _expenseAccount = Account "abn" "EUR"
          , _expenseCurrency = "EUR"
          , _expenseCategory = cat
          , _expenseTags = []
          , _expenseDay = dt
          , _expenseDescription = desc
          }
        liftIO $ trnAddedFire ()
      _ -> do
        pure ()
  pure ExpenseEditorWidget{..}

formatAmount :: Int -> String
formatAmount amt = show $ scientific (fromIntegral $ abs amt) (-2)

transactionsChanged :: ExpenseEditorWidget -> Event ()
transactionsChanged = _trnAddedBE
