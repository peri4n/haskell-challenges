module Problems.Problem32 where

import Data.List

solve :: IO Int
solve = return solution

solution :: Int
solution = sum $ nub [x*y | x <- [1..2000], y <- [1..50], isPanDigit x y]

isPanDigit :: Int -> Int -> Bool
isPanDigit x y = sort (show x ++ show y ++ show p) == "123456789"
  where
    p = x * y
