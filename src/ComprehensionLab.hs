module ComprehensionLab where

import Control.Arrow
import Data.Map.Strict as M

-- Task 0.8.1
increments :: Num a => [a] -> [a]
increments as = [ x + 1 | x <- as ]

-- Task 0.8.2
cubes :: Num a => [a] -> [a]
cubes as = [ x ^ 3 | x <- as ]

-- Task 0.8.3
tupleSum :: Num a => [(a,a)] -> [(a,a)] -> [(a,a)]
tupleSum a b = [ (x + x', y + y') | ((x, y), (x', y')) <- zip a b ]

-- Task 0.8.4
invDict :: (Ord k, Ord v) => Map k v -> Map v k
invDict m = M.fromList [ (v, k) | (k, v) <- M.toList m ]

-- Task 0.8.5
row :: Int -> Int -> [Int]
row p n = [ p + x | x <- [0..n-1] ]

grid :: [[Int]]
grid = [ row i w | i <- row 0 h ]
  where w = 5
        h = 3

grid' :: [[Int]]
grid' = [ [ i + j | j <- [0..w-1] ] | i <- [0..h-1] ]
  where w = 5
        h = 3

-- Task 0.8.6
-- Yes, invert the arrows

-- Task 0.8.7
-- No, the domain does not completely map onto the codomain

-- Task 0.8.8
-- g : R -> R ?

-- Task 0.8.9
-- No. We could define f . g as all the outputs of f map to inputs of g
-- All outputs of g do not map to the inputs of f therefore we cannot define g . f

-- Task 0.8.10
-- Odd: 0.2 + 0.1 = 0.3 = 3/10
-- Even: 1 - 0.3 = 0.7 = 7/10

-- Task 0.8.11
-- 0 -> 2 / 7
-- 1 -> 3 / 7
-- 2 -> 2 / 7

