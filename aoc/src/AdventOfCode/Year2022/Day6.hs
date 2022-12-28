module AdventOfCode.Year2022.Day6 where

import Data.Functor
import Data.List
import Data.Maybe
import Data.Set (fromList)
import qualified Data.Set as Set

-- solution

solveA :: IO Int
solveA = solve 4 <$> signals

solveB :: IO Int
solveB = solve 14 <$> signals

chunks :: Int -> [a] -> [[a]]
chunks k xs = if length xs < k then [] else take k xs : chunks k (tail xs)

solve :: Int -> String -> Int
solve k signals = i
  where
    (i, _) = fromJust $ find (\(_, s) -> Set.size s == k) igroups
    igroups = zip [k ..] groups
    hasDups xs = Set.size $ fromList xs
    groups = map fromList $ chunks k signals

-- Load and parse data

type Input = String

signals :: IO Input
signals = readFile "data/2022/day6.txt"
