module Control.Monad.Eff.Confirm where

import Control.Monad.Eff

foreign import data Confirm :: !

foreign import confirm
  "function confirm(msg) {\
  \  return function() {\
  \    return window.confirm(msg);\
  \  };\
  \}" :: forall eff. String -> Eff (confirm :: Confirm | eff) Boolean
