module AdventOfCode.Year2015.Day1 where

import Data.Functor
import Data.List

type Input = [Move]

type Floor = Int

data Move = Up | Down
  deriving (Show)

start :: Floor
start = 0

-- Solution
move :: Int -> Move -> Int
move f Up = f + 1
move f Down = f - 1

floors :: Input -> [Floor]
floors = scanl' move start

solveA :: IO Int
solveA = input <&> foldl' move start

solveB :: IO Int
solveB = input <&> length . takeWhile (/= -1) . floors

-- Load and parse input
input :: IO Input
input = do
  content <- readFile "data/2015/day1.txt"
  return $ map parseMove content

parseMove :: Char -> Move
parseMove '(' = Up
parseMove ')' = Down
