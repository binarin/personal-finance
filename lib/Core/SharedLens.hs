module Core.SharedLens where

import Control.Lens (Lens')

class HasAmount s a | s -> a where
  amount :: Lens' s a
