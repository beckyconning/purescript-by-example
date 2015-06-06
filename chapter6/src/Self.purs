module Self where

import Action
import Data.Monoid

newtype Self m = Self m

instance selfAction :: (Monoid m) => Action m (Self m) where
  act _ (Self x) = Self (x <> x)

instance showArraySelf :: (Show a) => Show (Self a) where
  show (Self x) = "(Self " ++ show x ++ ")"
