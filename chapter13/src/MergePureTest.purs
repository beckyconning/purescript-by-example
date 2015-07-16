module MergePureTest where

import Test.QuickCheck
import Merge
import Data.Array
import Data.Foldable
import Data.Function (on)
import Data.Maybe
import Data.Monoid.First

testAssoc = quickCheckPure 384103 10 f
  where
  f xs ys zs g = (xs `h` ys) `h` zs == xs `h` (ys `h` zs)
    where
    h xs ys = (mergeWith (numberToBool g)) (sortBy (compare `on` g) xs) (sortBy (compare `on` g) ys)

    numberToBool :: (Number -> Boolean) -> Number -> Boolean
    numberToBool = id

testAllAssoc = all f testAssoc
  where
  f Success = true
  f _       = false

testAllAssoc' = foldMap (<>) (First <$> (f <$> testAssoc))
  where
  f Success    = Nothing
  f (Failed s) = Just s
