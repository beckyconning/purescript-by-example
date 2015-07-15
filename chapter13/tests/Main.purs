module Main where

import Debug.Trace

import Data.Array
import Data.Function (on)
import Data.Foldable (foldr)
import Data.Char
import Data.String (fromCharArray)

import Math (floor)
import Merge
import Tree

import Test.QuickCheck
import Test.QuickCheck.Gen

isSorted :: forall a. (Ord a) => [a] -> Boolean
isSorted (x1 : t@(x2 : xs)) = x1 <= x2 && isSorted t
isSorted _ = true

isSubarrayOf :: forall a. (Eq a) => [a] -> [a] -> Boolean
isSubarrayOf xs ys = xs `intersect` ys == xs

data OneTwoThree a = One a | Two a a | Three a a a

instance coarbOneTwoThree :: (CoArbitrary a) => CoArbitrary (OneTwoThree a) where
  coarbitrary (One x) = coarbitrary x
  coarbitrary (Two x y ) = coarbitrary x <<< coarbitrary y
  coarbitrary (Three x y z) = coarbitrary x <<< coarbitrary y <<< coarbitrary z

newtype Byte = Byte Number

instance arbitraryByte :: Arbitrary Byte where
  arbitrary = uniformToByte <$> arbitrary
    where
    uniformToByte n = Byte $ Math.floor (n * 256)

instance coarbByte :: CoArbitrary Byte where
  coarbitrary (Byte x) = coarbitrary x

newtype Sorted a = Sorted [a]

sorted :: forall a. Sorted a -> [a]
sorted (Sorted xs) = xs

instance arbSorted :: (Arbitrary a, Ord a) => Arbitrary (Sorted a) where
  arbitrary = Sorted <<< sort <$> arbitrary

instance coarbSorted :: (CoArbitrary a) => CoArbitrary (Sorted a) where
  coarbitrary (Sorted xs) = coarbitrary xs

newtype LowercaseChar = LowercaseChar Char

instance arbLowercaseChar :: Arbitrary LowercaseChar where
  arbitrary = LowercaseChar <<< fromCharCode <<< uniformToLowercaseCharCode <$> arbitrary
    where
    uniformToLowercaseCharCode = Math.floor <<< (+ 97) <<< (* 26)

lowercaseChar :: LowercaseChar -> Char
lowercaseChar (LowercaseChar c) = c

newtype LowercaseString = LowercaseString String

instance arbLowercaseString :: Arbitrary LowercaseString where
  arbitrary = LowercaseString <<< fromCharArray <<< (lowercaseChar <$>) <$> arbitrary

instance showLowercaseString :: Show LowercaseString where
  show (LowercaseString s) = s

instance arbTree :: (Arbitrary a, Ord a) => Arbitrary (Tree a) where
  arbitrary = fromArray <<< sorted <$> arbitrary

instance coarbTree :: (CoArbitrary a) => CoArbitrary (Tree a) where
  coarbitrary Leaf = id
  coarbitrary (Branch l a r) =
    coarbitrary l <<<
    coarbitrary a <<<
    coarbitrary r

numbers :: [Number] -> [Number]
numbers = id

bools :: [Boolean] -> [Boolean]
bools = id

string :: String -> String
string = id

strings :: [String] -> [String]
strings = id

numberToBool :: (Number -> Boolean) -> Number -> Boolean
numberToBool = id

treeOfNumber :: Tree Number -> Tree Number
treeOfNumber = id

insertEach :: forall a. (Ord a) => [a] -> Tree a -> Tree a
insertEach xs tree = foldr insert tree xs

main = do
  -- Tests for module 'Merge'

  quickCheck $ \xs -> xs `merge` [] == xs
    <?> "Merging " ++ show xs ++ " with [] doesn't produce " ++ show xs ++ "."
  quickCheck $ \xs -> [] `merge` xs == xs
    <?> "Merging [] with " ++ show xs ++ " doesn't produce " ++ show xs ++ "."

  quickCheck $ \xs ys -> isSorted $ merge (sorted xs) (sorted ys)
    <?> "The result of merging these sorted arrays: " ++ show (sorted xs) ++ " and " ++ show (sorted ys) ++ " isn't sorted."
  quickCheck $ \xs ys -> xs `isSubarrayOf` merge xs ys
    <?> show xs ++ " is not a subarray of the result of merging " ++ show xs ++ " and " ++ show ys ++ "."

  quickCheck $ \xs ys -> isSorted $ numbers $ mergePoly (sorted xs) (sorted ys)
  quickCheck $ \xs ys -> numbers xs `isSubarrayOf` mergePoly xs ys

  quickCheck $ \xs ys -> isSorted $ bools $ mergePoly (sorted xs) (sorted ys)
  quickCheck $ \xs ys -> bools xs `isSubarrayOf` mergePoly xs ys

  quickCheck $ \xs ys f -> isSorted $ map f $ mergeWith (numberToBool f) (sortBy (compare `on` f) xs) (sortBy (compare `on` f) ys)
  quickCheck $ \xs ys f -> xs `isSubarrayOf` mergeWith (numberToBool f) xs ys


  -- Tests for module 'Tree'

  quickCheck $ \t a -> member a $ insert a (t :: Tree Number)
  quickCheck $ \t xs -> isSorted $ toArray $ foldr insert t $ numbers xs

  quickCheck $ \f g t ->
    anywhere (\s -> f s || g s) t ==
      anywhere f (treeOfNumber t) || anywhere g t

  quickCheck $ \x ys z -> (string x) `member` ((strings ys) `insertEach` (string x `insert` z))


  -- Tests for module 'Data.Array'

  quickCheck $ \n b -> (length $ replicate (floor (n * 256)) (bools b)) == (floor (n * 256))

  --
