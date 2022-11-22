module Problems.Problem3 where

import Data.IntMap (findMax)
import Numbers.Primes (primeFactors)

-- What is the largest prime factor of the number 600851475143 ?

solve :: IO Int
solve = return solution

solution :: Int
solution = fst $ findMax (primeFactors 600851475143)

