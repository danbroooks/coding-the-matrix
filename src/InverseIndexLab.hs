module InverseIndexLab where

import qualified Data.List       as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set        as S
import           System.Random

type WordMap = Map String (Set Int)

-- Task 0.6.2
movieReview :: String -> StdGen -> String
movieReview _ seed = reviews !! select
  where
    reviews = ["See it!", "A gem!", "Ideological claptrap!"]
    select = (fst . next $ seed) `mod` length reviews

-- Task 0.6.6
makeInverseIndex :: [String] -> Map String (Set Int)
makeInverseIndex = M.unionsWith S.union . concat . zipWith insert [0..] . L.map words
  where
    insert idx = map $ flip singleton idx

singleton :: String -> Int -> WordMap
singleton k v = M.singleton k (S.singleton v)

-- Task 0.6.7
orSearch :: (Ord k, Ord v) => [k] -> Map k (Set v) -> Set v
orSearch ws =
  M.foldr S.union S.empty . pick ws

-- Task 0.6.8
andSearch :: (Ord k, Ord v) => [k] -> Map k (Set v) -> Set v
andSearch ws index
  | M.null result = S.empty
  | otherwise   = foldr1 S.intersection result
  where
    result = pick ws index

pick :: Ord k => [k] -> Map k v -> Map k v
pick ks m =
  M.restrictKeys m $ S.fromList ks

storiesBig :: IO (Map String (Set Int))
storiesBig =
  loadFile "./src/InverseIndexLab/stories_big.txt"

storiesSmall :: IO (Map String (Set Int))
storiesSmall =
  loadFile "./src/InverseIndexLab/stories_small.txt"

loadFile :: String -> IO (Map String (Set Int))
loadFile path =
  makeInverseIndex . lines <$> readFile path

exampleSearch :: String -> IO (Set Int)
exampleSearch q =
  andSearch (words q) <$> storiesBig
