module Data.Hashable where

import Prelude hiding ((<#>))

import Data.Maybe
import Data.Foldable
import Data.Tuple
import Data.Either
import Data.String
import Data.Char

import Data.Function

type HashCode = Number

class (Eq a) <= Hashable a where
  hash :: a -> HashCode

(<#>) :: HashCode -> HashCode -> HashCode
(<#>) h1 h2 = (73 * h1 + 51 * h2) % 65536

hashEqual :: forall a. (Hashable a) => a -> a -> Boolean
hashEqual = (==) `on` hash

instance hashChar :: Hashable Char where
  hash = toCharCode

instance hashString :: Hashable String where
  hash = hash <<< toCharArray

instance hashNumber :: Hashable Number where
  hash n = hash (show n)

instance hashBoolean :: Hashable Boolean where
  hash false = 0
  hash true  = 1

instance hashArray :: (Hashable a) => Hashable [a] where
  hash [] = 0
  hash (x : xs) = hash x <#> hash xs

instance hashMaybe :: (Hashable a) => Hashable (Maybe a) where
  hash Nothing = 0
  hash (Just a) = 1 <#> hash a

instance hashTuple :: (Hashable a, Hashable b) => Hashable (Tuple a b) where
  hash (Tuple a b) = hash a <#> hash b

instance hashEither :: (Hashable a, Hashable b) => Hashable (Either a b) where
  hash (Left a) = 0 <#> hash a
  hash (Right b) = 1 <#> hash b

dups :: forall a. (Hashable a) => [a] -> Boolean
dups [] = false
dups (x : xs) = foldr f false xs || dups xs
  where
  f y acc = acc || ((hash x) == (hash y) && x == y)

newtype Uniform = Uniform Number

instance eqUniform :: Eq Uniform where
  (==) (Uniform u1) (Uniform u2) = u1 % 1.0 == u2 % 1.0
  (/=) (Uniform u1) (Uniform u2) = u1 % 1.0 /= u2 % 1.0

instance hashUniform :: Hashable Uniform where
  hash (Uniform x) = hash (x % 1.0)

testHashUniformEquality = hash (Uniform 10) == hash (Uniform 20)
testHashUniformInequality = hash (Uniform 10.1) /= hash (Uniform 20.1)

testHashMaybeEquality :: Boolean
testHashMaybeEquality = y && z
  where
  w = Nothing :: Maybe Number
  x = Just 10 :: Maybe Number
  y = hash w == hash w
  z = hash x == hash x

testHashMaybeInequality :: Boolean
testHashMaybeInequality = y && z
  where
  w = Nothing :: Maybe Number
  x = Just 10 :: Maybe Number
  y = hash w /= hash x
  z = hash x /= hash y
