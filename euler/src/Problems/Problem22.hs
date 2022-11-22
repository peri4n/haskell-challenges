module Problems.Problem22 where

import Data.List (sort)
import Data.Char (ord)
import System.IO

solve :: IO Int
solve = do
  content <- readFile "data/p022_names.txt"
  let ls = parse content
      names = zip (sort ls) [1..]
  return (sum $ map (\(x, y) -> numValue x * y) names)

numValue :: String -> Int
numValue str = sum $ map rank str
  where rank x = ord x - ord 'A' + 1

parse ::String -> [String]
parse str = words $ map repl $ filter (/= '"') str
  where repl x = if x == ',' then ' ' else x
