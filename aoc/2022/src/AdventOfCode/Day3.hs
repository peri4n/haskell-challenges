module AdventOfCode.Day3 where

import Data.Char (isLower, ord)
import Data.Functor ((<&>))
import Data.List (intersect)

-- solution
shared :: Input -> Char
shared is = head $ foldl1 intersect is

priority :: Char -> Int
priority x
  | isLower x = ord x - 96
  | otherwise = ord x - 64 + 26

solveA :: IO Int
solveA = items <&> map (priority . shared) <&> sum

solveB :: IO Int
solveB = groups <&> map (priority . shared) <&> sum

-- parse file
type Items = String

type Input = [Items]

items :: IO [Input]
items = do
  content <- readFile "data/day3.txt"
  return $ map parseItems $ lines content

parseItems :: String -> Input
parseItems items = [start, end]
  where
    (start, end) = splitAt half items
    half = length items `div` 2

type Group = [Items]

groupsOf :: Int -> [a] -> [[a]]
groupsOf n [] = []
groupsOf n xs = start : groupsOf n rest
  where 
    (start, rest) = splitAt n xs

groups :: IO [Group]
groups = do
  content <- readFile "data/day3.txt"
  return $ groupsOf 3 $ lines content
