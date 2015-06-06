module Exercises where

import Debug.Trace
import Data.Maybe
import Data.Array
import Data.Tuple
import Math
import Control.Monad
import Control.MonadPlus
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.ST
import Control.Monad.Eff.Random

third :: forall a. Array a -> Maybe a
third xs = do
  ys <- tail xs
  zs <- tail ys
  head zs

sums :: forall a. Array Number -> Array Number
sums = nub <<< sort <<< foldM (\x y -> [x, x+ y]) 0

-- > :t ((Just id) `ap`)
-- forall t4. Data.Maybe.Maybe t4 -> Data.Maybe.Maybe t4
-- > :t ((Just id) <*>)
-- forall t2. Data.Maybe.Maybe t2 -> Data.Maybe.Maybe t2

-- Left identity:
-- > let f = \x -> Just (x + 1)
-- > (return 1 :: Maybe Number) >>= f
-- Just (2)
-- > f 1
-- Just (2)

-- Right identity
-- > let m = (return 1 :: Maybe Number)
-- > m >>= return
-- Just (1)
-- > m
-- Just (1)

-- Associativity:
-- > let g = \x -> Just (x * 5)
-- > (m >>= f) >>= g
-- Just (10)
-- > m >>= (\x -> f x >>= g)
-- Just (10)

filterM' :: forall m a. (Monad m) => (a -> m Boolean) -> [a] -> m [a]
filterM' f (x : xs) = f x >>= \z -> filterM f xs >>= \xs' -> return (if z then x : xs' else xs')

-- Given that
-- (<$>) f m = m >>= \x -> return (f x)
-- lift2 f a b = f <$> a <*> b

-- lift2 f (return a) (return b) = return (f a b)

-- f <$> return a <*> return b

-- (<*>) (return a >>= \x -> return (f x)) (return b)

-- let f = return <<< f
-- (<*>) (return a >>= f) (return b)
-- (<*>) (f a) (return b)

-- (f a) <$> (return b)
-- (return b) >>= \x -> return (f a) x

-- let g = return <<< (f a)
-- (return b) >>= g
-- g b

-- return ((f a) b)
-- return (f a b)

safeDivide :: forall eff. Number -> Number -> Eff (err :: Exception | eff) Number
safeDivide a b | a % b == 0 = return (a / b)
safeDivide a b | a % b /= 0 = throwException $ error "Denominator does not divide numerator"

safeDivideTest :: Number -> Number -> Eff (trace :: Trace) Unit
safeDivideTest a b = catchException (trace <<< show) $ safeDivide a b >>= (trace <<< show)

estimatePi :: Number -> Eff (random :: Random) Number
estimatePi precision = runST (do
  pointsInCircleCountRef <- newSTRef 0
  forE 0 precision $ \_ -> do
    point <- generatePoint
    modifySTRef pointsInCircleCountRef ((+) $ boolToNumber $ isInCircle point)
    return unit
  pointsInCircleCount <- readSTRef pointsInCircleCountRef
  return $ (4 * pointsInCircleCount) / precision)
  where
  generatePoint :: forall eff. Eff (random :: Random | eff) (Tuple Number Number)
  generatePoint = random >>= \x -> random >>= \y -> return $ Tuple x y
  isInCircle (Tuple x y) = ((x - 0.5) `pow` 2) + ((y - 0.5) `pow` 2) < (0.5 `pow` 2)
  boolToNumber true  = 1
  boolToNumber false = 0
