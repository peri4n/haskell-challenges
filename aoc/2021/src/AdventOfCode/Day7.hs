module AdventOfCode.Day7 where

import Data.List.Split

positions :: IO [Int]
positions = map read . splitOn [','] . (head . lines) <$> readFile "data/day7.txt"

solve :: (Int -> Int -> Int) -> IO Int
solve dist = do
  pos <- positions
  let (min, max) = (minimum pos, maximum pos)
  let costs = map (\p -> sum $ map (dist p) pos) [min .. max]
  return $ minimum costs

solveA :: IO Int
solveA = solve dist
  where
    dist p sub = abs (p - sub)

solveB :: IO Int
solveB = solve dist
  where
    dist p sub = let n = abs (p - sub) in n * (n + 1) `div` 2
