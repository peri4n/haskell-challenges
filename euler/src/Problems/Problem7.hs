module Problems.Problem7 where

import Numbers.Primes

-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
-- What is the 10001st prime number?

solve :: IO Int
solve = return solution

solution :: Int
solution = filter isPrime [1..] !! 10000
