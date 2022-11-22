module Problems.Problem27 where

import Data.Foldable (maximumBy)
import Data.Ord (comparing)
import Numbers.Primes

solve :: IO Int
solve = return solution

solution :: Int
solution = p
  where
    (p, c) = maximumBy (comparing snd) allSeries

series :: Int -> Int -> [Int]
series a b = [n * n + a * n + b | n <- [0 ..]]

nPrimes :: Int -> Int -> Int
nPrimes a b = length $ takeWhile isPrime (series a b)

allSeries :: [(Int, Int)]
allSeries = [(a * b, count) | a <- [-999 .. 999], b <- [-1000 .. 1000], let count = nPrimes a b, count > 0]
