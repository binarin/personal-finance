module UICommon
  ( childrenM
  , makeAmountElement
  , divWithDate
  , block
  , bem
  , module UI
  , module Common
  ) where

import Common as Common

import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Core as UI
import           Graphics.UI.Threepenny.Core hiding (Config, row, column)
import           Graphics.UI.Threepenny.JQuery
import qualified Graphics.UI.Threepenny.Widgets as UI
import qualified Data.Text as T
import           Numeric (showFFloat)

childrenM :: WriteAttr Element [UI Element]
childrenM = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ i


formatExpenseAmount :: Int -> String
formatExpenseAmount amt = showFFloat (Just 2) ((fromIntegral amt :: Double) / 100) ""

formatCurrency :: Text -> String
formatCurrency "EUR" = "€"
formatCurrency "RUB" = "₽"
formatCurrency other = T.unpack other

makeAmountElement :: Int -> Text -> UI Element
makeAmountElement amount currency = UI.div #+ [UI.string $ formatExpenseAmount amount <> " " <> formatCurrency currency]

divWithDate :: Day -> UI Element
divWithDate day = UI.string $ yyyy_mm_dd day

bem :: Widget w => String -> w -> (String, Element)
bem elementName wdgt = (elementName, getElement wdgt)

block :: MonadUI m => String -> [(String, Element)] -> m Element
block blockName namedElements = do
    container <- liftUI $ UI.div # set UI.class_ blockName
    wrappedElements <- flip mapM namedElements $ \(elName, elt) -> do
        wrapper <- liftUI $ UI.div # set UI.class_ (blockName <> "__" <> elName)
                                   # set children [elt]
        pure wrapper
    liftUI $ pure container # set children wrappedElements
