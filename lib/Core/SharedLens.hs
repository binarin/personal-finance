module Core.SharedLens where

import Control.Lens (Lens')

class HasAmount s a | s -> a where
  amount :: Lens' s a

class HasDescription s a | s -> a where
  description :: Lens' s a

class HasDay s a | s -> a where
  day :: Lens' s a
