module AdventOfCode.Year2022.Day1 where

import Data.Functor
import Data.List
import Data.List.Split (splitOn)

-- solution

solveA :: IO Calories
solveA = calories <&> maximum

solveB :: IO Calories
solveB = calories <&> sort <&> reverse <&> take 3 <&> sum

-- Load and parse data

type Calories = Int

type Input = [Calories]

calories :: IO Input
calories = do
  content <- readFile "data/2022/day1.txt"
  return $ map (sum . map read) (splitOn [""] (lines content))
