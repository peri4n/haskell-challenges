module Problems.Problem41 where

import Numbers.Combinatorics (permute)
import Numbers.Primes (isPrime)
import Numbers.Series (pandigital1)

solve :: IO Int
solve = return solution

solution = maximum pali
  where pali = filter isPrime $ pandigital1 7

