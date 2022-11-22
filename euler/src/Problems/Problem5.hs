module Problems.Problem5 where

-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

solve :: IO Int
solve = return solution

solution :: Int
solution = foldl lcm 1 [2..20]


