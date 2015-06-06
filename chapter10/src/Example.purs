module Example where

import Debug.Trace
import Control.Monad.Eff.Confirm
import Control.Monad.Eff

main :: Eff (trace :: Trace, confirm :: Confirm) Unit
main = confirm "hi" >>= (trace <<< show) >>= \_ -> return unit
