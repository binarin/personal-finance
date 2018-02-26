module Common ( module Calendar
              , module X
              , yyyy_mm_dd
              )where

import Data.Time.Calendar as Calendar (Day)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Control.Monad as X (void)
import Data.Text as X (Text)
import Data.Monoid as X ((<>))
import Control.Lens as X ((^.))
import Data.Maybe as X (fromMaybe)


yyyy_mm_dd :: Day -> String
yyyy_mm_dd = formatTime defaultTimeLocale "%Y-%m-%d"
