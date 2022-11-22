module AdventOfCode.Day6 where

import Data.List as L (foldl', group, sort, take)
import Data.List.Split
import Data.Map.Lazy as M hiding (map)

solveA :: IO Int
solveA = simulate 80 <$> population

solveB :: IO Int
solveB = simulate 256 <$> population

simulate :: Int -> Pop -> Int
simulate gen pop =
  let p = iterate step pop !! gen
   in sum (elems p)

type Age = Int

type Count = Int

type Gen = (Age, Count)

type Pop = Map Age Count

step :: Pop -> Pop
step = foldlWithKey' addGen empty
  where
    addGen p a c = L.foldl' (\acc x -> uncurry (insertWith (+)) x acc) p (age a c)

age :: Age -> Count -> [Gen]
age 0 c = [(6, c), (8, c)]
age x c = [(x -1, c)]

population :: IO Pop
population = groupByAge . map read . splitOn [','] . (head . lines) <$> readFile "data/day6.txt"
    where groupByAge as = fromList $ map (\a -> (head a, length a)) (group $ sort as)
