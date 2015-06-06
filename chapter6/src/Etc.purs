module Data.Etc where

import Data.Maybe
import Data.Tuple
import Data.Either
import Data.Char
import Data.Array
import Prelude hiding ((<#>))
import Data.Foldable
import Data.Monoid (mempty)

newtype Complex = Complex { real :: Number, imaginary :: Number }

instance showComplex :: Show Complex where
  show (Complex x) = "Real: " ++ (show x.real) ++ ", Imaginary: " ++ (show x.imaginary)

instance eqComplex :: Eq Complex where
  (==) (Complex x) (Complex y) = (x.real == y.real) && (x.imaginary == y.imaginary)
  (/=) (Complex x) (Complex y) = (x.real /= y.real) || (x.imaginary /= y.imaginary)

data NonEmpty a = NonEmpty a [a]

instance semigroupNonEmpty :: (Semigroup a) => Semigroup (NonEmpty a) where
  (<>) (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs <> [y] <> ys)

instance showNonEmpty :: (Show a) => Show (NonEmpty a) where
  show (NonEmpty x xs) = show ([x] <> xs)

instance functorNonEmpty :: Functor NonEmpty where
  (<$>) f (NonEmpty x xs) = NonEmpty (f x) (f <$> xs)

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr f z (NonEmpty x xs) = foldr f z (x : xs)
  foldl f z (NonEmpty x xs) = foldl f z (x : xs)
  foldMap f (NonEmpty x xs) = foldMap f (x : xs)

instance eqNonEmpty :: (Eq a) => Eq (NonEmpty a) where
  (==) (NonEmpty x xs) (NonEmpty y ys) = (x : xs) == (y : ys)
  (/=) (NonEmpty x xs) (NonEmpty y ys) = (x : xs) /= (y : ys)

data Extended a = Finite a | Infinite

instance showExtended :: (Show a) => Show (Extended a) where
  show Infinite = "Infinite"
  show (Finite x) = "(Finite " ++ show x ++ ")"

instance eqExtended :: (Eq a) => Eq (Extended a) where
  (==) (Finite x) (Finite y) = x == y
  (==) (Finite _) Infinite   = false
  (==) Infinite (Finite _)   = false
  (==) Infinite Infinite     = false
  (/=) (Finite x) (Finite y) = x /= y
  (/=) (Finite _) Infinite   = true
  (/=) Infinite (Finite _)   = true
  (/=) Infinite Infinite     = true

instance ordExtended :: (Ord a) => Ord (Extended a) where
  compare (Finite x) (Finite y) = compare x y
  compare (Finite _) Infinite = LT
  compare Infinite (Finite _) = GT
  compare Infinite Infinite = EQ

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: (Foldable f) => Foldable (OneMore f) where
  foldr f z (OneMore x ys) = f x (foldr f z ys)
  foldl f z (OneMore x ys) = f (foldl f z ys) x
  foldMap f (OneMore x ys) = (f x) <> (foldMap f ys)
