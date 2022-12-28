module AdventOfCode.Year2020.Day2 where

import Data.Functor ((<&>))

cases :: IO [Case]
cases = do
  content <- readFile "data/2020/day2.txt"
  return $ map read (lines content)

solveA :: IO Int
solveA = cases <&> (length . filter validA)

solveB :: IO Int
solveB = cases <&> (length . filter validB)

validA :: Case -> Bool
validA (Case (Policy min max char) test) = (min <= occ) && (occ <= max)
  where
    occ = count char test

validB :: Case -> Bool
validB (Case (Policy pos1 pos2 char) test) = (test !! (pos1 - 1) == char) `xor` (test !! (pos2 -1) == char)

count c xs = length $ filter (== c) xs

data Policy = Policy Int Int Char
  deriving (Show)

data Case = Case Policy String
  deriving (Show)

instance Read Case where
  readsPrec _ str =
    let (policy, test) = split ':' str
     in [(Case (read policy) (drop 2 test), "")]

instance Read Policy where
  readsPrec _ str = [(Policy min max c, "")]
    where
      (range, char) = split ' ' str
      (m, rest) = split '-' range
      c = char !! 1
      min = read m
      max = read (tail rest)

split :: (Eq a) => a -> [a] -> ([a], [a])
split s xs = (policy, test)
  where
    (policy, test) = span (/= s) xs

xor :: Bool -> Bool -> Bool
True  `xor` False = True
False `xor` True  = True
_     `xor` _     = False
