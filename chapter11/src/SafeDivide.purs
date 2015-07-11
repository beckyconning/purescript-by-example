module SafeDivide where

import Control.Monad.Error.Trans
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Error.Class

safeDivide :: Number -> Number -> ErrorT String Identity Number
safeDivide x 0 = throwError "Denominator is 0"
safeDivide x y = return $ x / y
