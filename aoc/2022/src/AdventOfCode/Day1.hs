module AdventOfCode.Day1 where

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
  content <- readFile "data/day1.txt"
  return $ map (sum . map read) (splitOn [""] (lines content))
