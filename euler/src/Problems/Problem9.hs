module Problems.Problem9 where

import Data.List
import Numbers.Series

-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.  Find the product abc.

solve :: IO Int
solve = return solution

solution :: Int
solution =
    let trip = find (\(a,b,c) -> a + b + c == 1000) pyTriplets
     in case trip of
          Nothing -> 0
          Just (a,b,c) -> a * b * c
