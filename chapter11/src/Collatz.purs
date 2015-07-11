module Collatz where

import Data.Foldable (traverse_)
import Control.Monad.Writer
import Control.Monad.Writer.Class
import Data.Tuple

g :: Number -> Tuple Number [String]
g = runWriter <<< h 0
  where
  h :: Number -> Number -> Writer [String] Number
  h x 1 = return x
  h x y = tell ["f " ++ show y] >>= \_ -> h (x + 1) (f y)
    where
    f :: Number -> Number
    f x | x % 2 == 0 = x / 2
    f x              = 3 * x + 1
