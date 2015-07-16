module LowerCaseChar where

import Data.Char
import Data.String
import Test.QuickCheck
import Test.QuickCheck.Gen

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
