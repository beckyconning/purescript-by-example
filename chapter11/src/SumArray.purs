module SumArray where

import Data.Foldable (traverse_)
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.Writer
import Control.Monad.Writer.Class
import Data.Monoid.Additive

sumArray :: [Number] -> State Number Unit
sumArray = traverse_ $ \n -> modify (\sum -> sum + n)

sumArray' :: [Number] -> Writer (Additive Number) Unit
sumArray' = traverse_ $ \n -> tell (Additive n)

-- > runState (do
--     sumArray [1,2,3]
--     sumArray [4,5]
--     sumArray [6]
--   ) 9
--
-- Tuple (Unit {}) (30)


-- > execState (do
--     sumArray [1,2,3]
--     sumArray [4,5]
--     sumArray [6]
--   ) 0
--
-- 21

-- > evalState (do
--     sumArray [1,2,3]
--     sumArray [4,5]
--     sumArray [6]
--   ) 0
--
-- Unit {}
