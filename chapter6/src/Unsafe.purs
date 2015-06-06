module Unsafe where

import Data.Array hiding (last)

class Unsafe

foreign import unsafeIndex
  """
  function unsafeIndex() {
    return function (xs) {
      return function (n) {
        return xs[n];
      };
    };
  }
  """ :: forall a. (Unsafe) => [a] -> Number -> a

last :: forall a. (Unsafe) => [a] -> a
last xs = unsafeIndex xs ((length xs) - 1)
