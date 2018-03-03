{-# LANGUAGE RecordWildCards #-}
module UI.CategorySelector
  ( css
  , mkCategorySelector
  , userSelectedCategory
  ) where

import Data.Aeson as JSON
import UICommon
import qualified UIQual as UI
import UI.CategorySelectorCSS (css)

import qualified Data.Text as T

import Core.Account
import Service.Account as SvcAcc
import Graphics.UI.Threepenny.Widgets (ListBox, listBox, userSelection)

data CategorySelector = CategorySelector
  { _elementCS :: !Element
  , _selectionCS :: !(Tidings (Maybe Category))
  }

instance Widget CategorySelector where getElement = _elementCS

mkCategorySelector :: SvcAcc.Handle -> Behavior CategoryType -> Behavior (Maybe Category) -> UI CategorySelector
mkCategorySelector svcAcc catTypeB preselectedB = do
  initialType <- currentValue catTypeB
  initialCategories <- liftIO $ SvcAcc.getCategories svcAcc initialType

  (gotCategoriesE, fireGotCategories) <- liftIO newEvent
  categoriesB <- accumB initialCategories (const <$> gotCategoriesE)

  lb <- listBox categoriesB preselectedB (pure renderOption)

  let _elementCS = getElement lb
      _selectionCS = userSelection lb

  pure CategorySelector {..}

renderOption :: Category -> UI Element
renderOption cat = do
  opt <- UI.option
    # set UI.value (T.unpack $ cat^.name)
    #+ [UI.string $ T.unpack $ cat^.name]
  pure opt

properlySelected = fromJQueryProp "checked" (== JSON.Bool False) (JSON.Bool . not)

userSelectedCategory :: CategorySelector -> Tidings (Maybe Category)
userSelectedCategory = _selectionCS
