module AdventOfCode.Day1 where

import Data.Functor
import Data.List

numbers :: IO [Int]
numbers = do
  content <- readFile "data/day1.txt"
  return $ map read (lines content)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

slide :: Int -> [a] -> [[a]]
slide k xs =
  let window = take k xs
   in if length window < k then [] else window : slide k (tail xs)

slideSum :: (Num a) => Int -> [a] -> [a]
slideSum k xs = map sum $ slide k xs

solveA :: IO Int
solveA = numbers <&> incDec

solveB :: IO Int
solveB = numbers <&> (incDec . slideSum 3)

incDec :: (Integral a) => [a] -> Int
incDec ws = count (> 0) $ zipWith (-) (tail ws) ws
