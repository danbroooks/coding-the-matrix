module DictUtil where

import Data.Map as M

-- Task 0.5.30 / 0.6.3
dict2list :: Ord a => Map a b -> [a] -> [b]
dict2list dct keylist = [ dct ! k | k <- keylist ]

-- Task 0.5.31 / 0.6.3
list2dict :: Ord a => [b] -> [a] -> Map a b
list2dict l keylist = M.fromList $ zip keylist l

-- Task 0.6.4
listRange2Dict :: [a] -> Map Int a
listRange2Dict l = list2dict l [0..length l]

