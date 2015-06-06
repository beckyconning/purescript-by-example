module Stream where

import Data.Maybe
import Data.Tuple
import Data.String hiding (uncons)
import Data.Monoid

class Stream list element where
  uncons :: list -> Maybe (Tuple element list)

instance streamArray :: Stream [a] a where
  uncons [] = Nothing
  uncons (x : xs) = Just (Tuple x xs)

instance streamString :: Stream String String where
  uncons "" = Nothing
  uncons s = Just (Tuple (take 1 s) (drop 1 s))

foldStream :: forall l e m. (Stream l e, Monoid m) => (e -> m) -> l -> m
foldStream f list =
  case uncons list of
    Nothing -> mempty
    Just (Tuple head tail) -> f head <> foldStream f tail
