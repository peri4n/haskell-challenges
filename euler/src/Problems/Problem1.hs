module Problems.Problem1 where

solve :: IO Int
solve = return solution

solution :: Int
solution = sum [ x | x <- [1..999], x `mod` 3 == 0 ||  x `mod` 5 == 0]

