module FileOperations where

import Data.Path
import Data.Array
import Data.Foldable
import Control.MonadPlus
import qualified Data.Array.Unsafe as Uns

allFiles :: Path -> [Path]
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> [Path]
allFiles' file = file : do
  child <- ls file
  allFiles' child

factors :: Number -> [[Number]]
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  return [i, j]

repeat :: forall a. a -> Number -> [a]
repeat = repeatAcc []
  where
  repeatAcc :: forall a. [a] -> a -> Number -> [a]
  repeatAcc acc _ 0 = acc
  repeatAcc acc x n = repeatAcc (x : acc) x (n - 1)

-- Exercise 1.1
isEven :: Number -> Boolean
isEven n =
  if n % 2 == 0
  then true
  else false

-- Exercise 1.2
evenCount :: [Number] -> Number
evenCount [] = 0
evenCount xs = (if (isEven $ Uns.head xs) then 1 else 0) + (evenCount $ Uns.tail xs)

-- Exercise 2.1
squareArray :: [Number] -> [Number]
squareArray = map (\x -> x * x)

-- Exercise 2.2
withoutNegative :: [Number] -> [Number]
withoutNegative = filter (\x -> x >= 0)

-- Exercise 2.3
(<$?>) :: forall a. (a -> Boolean) -> [a] -> [a]
(<$?>) = filter

infix 20 <$?>

-- Exercise 3.1
isPrime :: Number -> Boolean
isPrime = ((==) 1) <<< length <<< factors

-- Exercise 3.2
cartesianProduct :: forall a. [a] -> [a] -> [[a]]
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  return [x, y]

-- Exercise 3.3
pythagoreanTriple :: Number -> [[Number]]
pythagoreanTriple n = do
  a <- 1 .. n
  b <- 1 .. n
  c <- 1 .. n
  guard $ (a * a) + (b * b) == (c * c)
  return [a, b, c]


-- Exercise 3.4
-- Too hard / esoteric


-- Exercise 3.5
-- Too hard / esoteric


-- Exercise 4.1
allTrue :: [Boolean] -> Boolean
allTrue = foldl (\m x -> m && x) true

-- Exercise 4.2
-- the function (foldl (==) false) will return true when
-- called on any array that has `false` at it's first index
-- and `true` at every other index


-- Exercise 4.3
count :: forall a. (a -> Boolean) -> [a] -> Number
count _ [] = 0
count p (x : xs) = if p x then 1 + count p xs else count p xs

count' :: forall a. (a -> Boolean) -> [a] -> Number
count' = countAcc 0
  where
  countAcc :: forall a. Number -> (a -> Boolean) -> [a] -> Number
  countAcc a _ [] = a
  countAcc a p (x : xs) = countAcc (if p x then a + 1 else a) p xs


-- Exercise 4.4
reverse' :: forall a. [a] -> [a]
reverse' = foldl (\m x -> x : m) []


-- Exercise 5.x
-- Todo
