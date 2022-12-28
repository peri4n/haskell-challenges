module AdventOfCode.Year2020.Day1 where

import Data.List

numbers :: IO [Int]
numbers = do
  content <- readFile "data/2020/day1.txt"
  return $ map read (lines content)

combinations :: Int -> [Int] -> [[Int]]
combinations k xs = filter ((k ==) . length . nub) $ mapM (const xs) [1 .. k]

solve :: Int -> IO Int
solve k = do
  ns <- numbers
  let solution = [product n | n <- combinations k ns, sum n == 2020]
  return $ head solution

solveA :: IO Int
solveA = solve 2

solveB :: IO Int
solveB = solve 3
