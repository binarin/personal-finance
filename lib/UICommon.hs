module UICommon
  ( childrenM
  , module UI
  , module Common
  ) where

import Common as Common

import Control.Monad (void)

import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Core as UI
import           Graphics.UI.Threepenny.Core hiding (Config, row, column)
import           Graphics.UI.Threepenny.JQuery
import qualified Graphics.UI.Threepenny.Widgets as UI

childrenM :: WriteAttr Element [UI Element]
childrenM = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ i
