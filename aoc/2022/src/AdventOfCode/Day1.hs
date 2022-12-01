module AdventOfCode.Day1 where

import Data.Functor
import Data.List
import Data.List.Split (splitOn)

calories :: IO [Int]
calories = do
  content <- readFile "data/day1.txt"
  return $ map (sum . map read) (splitOn [""] (lines content))

solveA :: IO Int
solveA = calories <&> maximum

solveB :: IO Int
solveB = calories <&> sort <&> reverse <&> take 3 <&> sum
