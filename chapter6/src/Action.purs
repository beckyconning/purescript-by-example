module Action where

import Data.Monoid
import Data.Array

class (Monoid m) <= Action m a where
  act :: m -> a -> a

instance arrayAction :: (Monoid m, Action m a) => Action m [a] where
  act x y = map (act x) y
