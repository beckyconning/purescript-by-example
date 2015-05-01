module FileOperations where

import Data.Path
import Data.Array
import Data.Tuple
import Data.Foldable
import Control.MonadPlus
import Data.Maybe

allFiles :: Path -> [Path]
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> [Path]
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> [Path]
onlyFiles = filter isFile <<< allFiles
  where
  isFile = not <<< isDirectory

findLargestFile :: Path -> Maybe Path
findLargestFile = foldl largestFile Nothing <<< onlyFiles
  where
  largestFile (Just x) y = if (size x) > (size y) then Just x else Just y
  largestFile Nothing y = Just y

findSmallestFile :: Path -> Maybe Path
findSmallestFile = foldl smallestFile Nothing <<< onlyFiles
  where
  smallestFile (Just x) y = if (size x) < (size y) then Just x else Just y
  smallestFile Nothing y = Just y

whereIs :: String -> Maybe Path
whereIs query = head $ allFiles root
  >>= \path  -> ls path
  >>= \child -> guard (filename child == query)
  >>= \_     -> return path

isEven :: Number -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = isEven $ n - 2

evenCount :: [Number] -> Number
evenCount [] = 0
evenCount (x:xs) = (boolBin $ isEven x) + evenCount xs
  where
  boolBin :: Boolean -> Number
  boolBin true = 1
  boolBin false = 0

squares :: [Number] -> [Number]
squares = map (\ n -> n * n)

(<$?>) = filter

removeNegatives :: [Number] -> [Number]
removeNegatives x = (\n -> n >= 0) <$?> x

factors :: Number -> [[Number]]
factors n = do
  i <- range 1 n
  j <- range i n
  guard $ i * j == n
  return [i, j]

-- factors' :: Number -> [[Number]]
-- factors' n = map factorMultiplicandPair $ filter isFactor (1 .. n)
--   where
--   isFactor :: Number -> Boolean
--   isFactor i = any (\ j -> j * i == n) (i .. n)
--   factorMultiplicandPair :: Number -> [Number]
--   factorMultiplicandPair x = [x, n / x]


actualFactors :: Number -> [Number]
actualFactors n = do
  i <- 1 .. n
  guard $ n % i == 0
  return i

factorizations :: Number -> [[Number]]
factorizations n = [n] : (actualFactors n
  >>= \ x -> guard (x > 1 && x < n)
  >>= \ _ -> factorizations (n / x)
  >>= \ ys -> return (x : ys))

cartesianProduct :: forall a b. [a] -> [b] -> [Tuple a b]
cartesianProduct xs ys = do
  i <- xs
  j <- ys
  [Tuple i j]

triples :: Number -> [[Number]]
triples n = do
  i <- range 1 n
  j <- range 1 n
  k <- range 1 n
  guard $ i * i + j * j == k * k
  return [i, j, k]

triples' :: Number -> [[Number]]
triples' n = range 1 n
  >>= \i -> range 1 n
  >>= \j -> range 1 n
  >>= \k -> guard (i * i + j * j == k * k)
  >>= return [[i, j, k]]

xfactors :: Number -> [Number]
xfactors n = do
  i <- 1 .. n
  guard $ n % i == 0
  return i

xfactorizations :: Number -> [[Number]]
xfactorizations n = [n] : do
  x <- xfactors n
  guard $ x > 1 && x < n
  xs <- xfactorizations (n / x)
  return (x : xs)

countB :: forall a. (a -> Boolean) -> [a] -> Number
countB p = countB' 0
  where
  countB' acc [] = acc
  countB' acc (x : xs) = if p x then countB' (acc + 1) xs else countB' acc xs

reverse' :: forall a. [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []
