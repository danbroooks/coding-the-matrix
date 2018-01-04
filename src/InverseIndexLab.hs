module InverseIndexLab where

import Data.List as L
import Data.Map.Strict as M
import Data.Set as S
import System.Random

-- Task 0.6.2
movieReview :: String -> StdGen -> String
movieReview _ seed = reviews !! select
  where
    reviews = ["See it!", "A gem!", "Ideological claptrap!"]
    select = (fst . next $ seed) `mod` length reviews

-- Task 0.6.6
makeInverseIndex :: [String] -> Map String (Set Int)
makeInverseIndex = L.foldr (unionWith S.union . uncurry group) M.empty . zipWithIndex . L.map words
  where
    zipWithIndex :: [a] -> [(Int, a)]
    zipWithIndex xs = zip [0..length xs] xs

    group :: Ord k => v -> [k] -> Map k (Set v)
    group n = M.fromList . L.map (\w -> (w, S.singleton n))

-- Task 0.6.7
orSearch :: [String] -> Map String (Set Int) -> Set Int
orSearch ws =
  M.foldr S.union S.empty . pick ws

-- Task 0.6.8
andSearch :: [String] -> Map String (Set Int) -> Set Int
andSearch ws index =
  M.foldr S.intersection (S.fromList [0..length index-1]) . pick ws $ index

pick :: Ord k => [k] -> Map k v -> Map k v
pick ks m =
  restrictKeys m $ S.fromList ks

storiesBig :: IO [String]
storiesBig =
  lines <$> readFile "./InverseIndexLab/stories_big.txt"

storiesSmall :: IO [String]
storiesSmall =
  lines <$> readFile "./InverseIndexLab/stories_small.txt"

