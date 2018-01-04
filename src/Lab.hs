module Lab where

import Data.List as L
import Data.Map as M
import Data.Set as S

-- Task 0.5.1
minutesInWeek :: Num a => a
minutesInWeek = 60 * 24 * 7

-- Task 0.5.2
remainderWithoutMod :: Integral a => a
remainderWithoutMod = 2304811 `quot` 47

-- Task 0.5.3
divisibleBy3 :: Bool
divisibleBy3 = 673 + 909 `mod` 3 == 0

-- Task 0.5.4
statementVal :: Floating a => a
statementVal =
  if x + 10 < 0 then 2 ** y + 0.5 else 2 ** y - 0.5
  where
    x = -9
    y = 0.5

-- Task 0.5.5
firstFiveSquares :: (Floating a, Enum a, Ord a) => Set a
firstFiveSquares =
  S.fromList [ x ** 2 | x <- [1..5] ]

-- Task 0.5.6
firstFivePowsTwo :: (Floating a, Enum a, Ord a) => Set a
firstFivePowsTwo =
  S.fromList [ 2 ** x | x <- [0..4] ]

-- Task 0.5.7
nineElementSet :: (Floating a, Enum a, Ord a) => Set a
nineElementSet = S.fromList [ x * y | x <- [5, 7, 9], y <- [20, 25, 30] ]

-- Task 0.5.8
fiveElementSet :: (Floating a, Enum a, Ord a) => Set a
fiveElementSet = S.fromList [ x * y | x <- [0, 2, 4], y <- [-1, 0, 1], x /= y ]

-- Task 0.5.9
sIntersectT :: Set Integer
sIntersectT = s `S.intersection` t
  where
    s = S.fromList [1..4]
    t = S.fromList [3..6]

-- Task 0.5.10
average :: Fractional b => b
average = total / amount
  where
    total = realToFrac $ sum list
    amount = L.genericLength list
    list = [20, 10, 15, 75]

-- Task 0.5.11
doubleComprehension :: [(Char, Int)]
doubleComprehension =
  [ (l, n) | l <- ['A', 'B', 'C'], n <- [1,2,3] ]

-- Task 0.5.12
sumListOfList :: Fractional a => a
sumListOfList = L.foldr ((+) . sum) 0 list
  where list = [[0.25, 0.75, 0.1], [-1, 0], [4, 4, 4, 4]]

-- Task 0.5.13
--
--   let [x,y,z] = [4*1, 4*2, 4*3, 4*4]
--   Haskell will error: 'Irrefutable pattern failed for pattern'

-- Task 0.5.14
zeroSumList :: [(Int, Int, Int)]
zeroSumList = [ (i, j, k) | i <- s, j <- s, k <- s, i + j + k == 0 ]
  where
    s = [-4, -2, 1, 2, 5, 0]

-- Task 0.5.15
excludeZeroList :: [(Int, Int, Int)]
excludeZeroList = [ (i, j, k) | i <- s, j <- s, k <- s, i + j + k == 0, (i,j,k) /= (0,0,0) ]
  where
    s = [-4, -2, 1, 2, 5, 0]

-- Task 0.5.16
firstOfTuplesList :: Maybe (Int, Int, Int)
firstOfTuplesList =
  case excludeZeroList of
    h : _ -> Just h
    []    -> Nothing

-- Task 0.5.17
uniqueConstraint :: Bool
uniqueConstraint = (length l) /= (length . S.toList . S.fromList $ l)
  where l = [0,1,0]

-- Task 0.5.18
oddNumListRange :: Set Int
oddNumListRange = S.fromList $ [ x | x <- [1..99], x `mod` 2 /= 0 ]

-- Task 0.5.19
rangeAndZip :: [(Int, Char)]
rangeAndZip = zip [0..n-1] l
  where l = ['A', 'B', 'C', 'D', 'E']
        n = length l

-- Task 0.5.20
listSumZip :: [Int]
listSumZip = [ x + y | (x, y) <- zip l1 l2 ]
  where l1 = [10, 25, 40]
        l2 = [1, 15, 20]

-- Task 0.5.21
valueList :: [String]
valueList = [ m ! k | m <- dlist]
  where
    dlist = [ M.fromList [("James", "Sean"), ("director", "Terence")]
            , M.fromList [("James", "Roger"), ("director", "Lewis")]
            , M.fromList [("James", "Pierce"), ("director", "Roger")]
            ]
    k = "James"

-- Task 0.5.22
valueList' :: String -> [Maybe String]
valueList' k = [ m !? k | m <- dlist]
  where
    dlist = [ M.fromList [("Bilbo", "Ian"), ("Frodo", "Elijah")]
            , M.fromList [("Bilbo", "Martin"), ("Thorin", "Richard")]
            ]

-- Task 0.5.23
squareDict :: Map Int Int
squareDict = M.fromList [ (x, x * x) | x <- [0..99] ]

-- Task 0.5.24
identityDict :: Map String String
identityDict = M.fromList [ (x, x) | x <- ["red", "white", "blue"] ]

-- Task 0.5.25
representationDict :: Int -> Map Int [Int]
representationDict base = M.fromList [ (x, encode x) | x <- digits ]
  where
    encode x =
      [ x `quot` base^2 `mod` base
      , x `quot` base `mod` base
      , x `mod` base
      ]
    digits = [0..base^3 - 1]

-- Task 0.5.26
listDictToDict :: Fractional num => Map String num
listDictToDict = M.fromList [ (name, id2Salary ! i) | (i, name) <- zip [0..length names - 1] names ]
  where
    names = ["Larry", "Curly", "Moe"]
    id2Salary = M.fromList [(0, 1000.0), (2, 990), (1, 1200.50)]

-- Task 0.5.27
class Twiceable a where
  twice :: a -> a

instance Twiceable Int where
  twice n = n * 2

instance Twiceable [a] where
  twice ls = ls ++ ls

-- Task 0.5.28
nextInts :: [Int] -> [Int]
nextInts = L.map (+ 1)

-- Task 0.5.29
cubes :: Num a => [a] -> [a]
cubes = L.map (^ 3)

-- Task 0.5.30
dict2list :: Ord a => Map a b -> [a] -> [b]
dict2list dct keylist = [ dct ! k | k <- keylist ]

-- Task 0.5.31
list2dict :: Ord a => [b] -> [a] -> Map a b
list2dict l keylist = M.fromList $ zip keylist l

-- Task 0.5.32
all3DigitNumbers :: Int -> Set Int
all3DigitNumbers base = S.fromList [0..base^3-1]
