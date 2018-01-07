module TheField.Problems where

import           Data.Set
import qualified Data.Set as S

-- Problem 1.7.4
newtype Sum a = Sum { unSum :: a }

instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    Sum x `mappend` Sum y = Sum (x + y)

mySum :: Num a => [a] -> a
mySum = unSum . mconcat . fmap Sum

-- Problem 1.7.5
newtype Product a = Product { unProduct :: a }

instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)

myProduct :: Num a => [a] -> a
myProduct = unProduct . mconcat . fmap Product

-- Problem 1.7.6
newtype Min a = Min { unMin :: a }

instance (Bounded a, Ord a, Num a) => Monoid (Min a) where
    mempty = Min maxBound
    Min x `mappend` Min y = Min (x `min` y)

myMin :: (Bounded a, Ord a, Num a) => [a] -> a
myMin = unMin . mconcat . fmap Min

-- Problem 1.7.7
newtype Concat = Concat { unConcat :: String }

instance Monoid Concat where
  mempty = Concat ""
  Concat x `mappend` Concat y = Concat (x ++ y)

myConcat :: [String] -> String
myConcat = unConcat . mconcat . fmap Concat

-- Problem 1.7.8
newtype Union a = Union { unUnion :: Set a }

instance Ord a => Monoid (Union a) where
  mempty = Union (S.fromList [])
  Union x `mappend` Union y = Union (x `S.union` y)

myUnion :: Ord a => [Set a] -> Set a
myUnion = unUnion . mconcat . fmap Union

-- Problem 1.7.9
-- > mySum [] == 0
-- > myProduct [] == 1
-- > myMin ([] :: [Int]) == 9223372036854775807 == maxBound
-- > myConcat [] == ""
-- > myUnion (S.fromList []) == S.fromList [])
