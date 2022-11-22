module Problems.Problem37 where

import Data.List
import Numbers.Primes (isPrime, primes)

solve :: IO Int
solve = return solution

solution :: Int
solution = sum $ take 11 $ filter isTruncPrime candidates
  where candidates = drop 4 primes

leftToRight :: Int -> [Int]
leftToRight x = map read $ init $ tails (show x)

rightToLeft :: Int -> [Int]
rightToLeft x = map read $ drop 1 $ inits (show x)

isTruncPrime :: Int -> Bool
isTruncPrime x = all isPrime (leftToRight x ++ rightToLeft x)
