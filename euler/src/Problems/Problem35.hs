module Problems.Problem35 where

import Data.List
import Numbers.Primes (primes, isPrime)

solve :: IO Int
solve = return solution

solution :: Int
solution = length [ p | p <- takeWhile (<1000000) primes, let r = rotate p, allPrimes r]

allPrimes :: [Int] -> Bool
allPrimes = all isPrime 

rotate :: Int -> [Int]
rotate n = map read $ take l (map (take l) starts)
  where str = show n
        l = length str
        starts = tails (cycle str)
