module Problems.Problem50 where

import Numbers.Primes (isPrime, primes)
import Data.Foldable (maximumBy)
import Data.Ord (comparing)

solve :: IO Int
solve = return solution

solution :: Int
solution = snd $ maximumBy (comparing fst) [ (i2 - i1, p2 - p1) | (i1, p1) <- pIntervals, (i2, p2) <- pIntervals, i2 > i1, isPrime (p2 - p1)]
  where pSums = takeWhile (<1000000) $ scanl1 (+) primes
        pIntervals = (0, 0): zip [1..] pSums
