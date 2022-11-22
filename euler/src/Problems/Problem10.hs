module Problems.Problem10 where

import Numbers.Primes

-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
-- Find the sum of all the primes below two million.

solve :: IO Int
solve = return solution

solution :: Int
solution = sum (takeWhile (<2000000) primes)
