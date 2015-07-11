module SetTimeout where

import Debug.Trace
import Data.Function
import Control.Monad.Eff
import Control.Monad.Trans
import Control.Monad.Cont.Trans

type Milliseconds = Number
type TimeoutEff eff = Eff (timeout :: Timeout | eff)

foreign import data Timeout :: !

foreign import setTimeoutForeign
  "function setTimeoutForeign(n, f) {\
  \  return function() {             \
  \    return setTimeout(f, n);      \
  \    return {};                    \
  \  }                               \
  \}" :: forall eff a. Fn2 Milliseconds (Eff a Unit) (Eff (timeout :: Timeout | eff) Unit)

setTimeoutCont :: forall eff. Milliseconds -> ContT Unit (TimeoutEff eff) Unit
setTimeoutCont n = ContT $ f
  where
  f m = (runFn2 setTimeoutForeign) n (m unit)

example = do
  setTimeoutCont 2500
  lift $ print "Hi"
