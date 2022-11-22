module Problems.Problem15 where

-- Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
-- How many such routes are there through a 20×20 grid?

solve :: IO Int
solve = return solution

solution :: Int
solution = iterate (scanl1 (+)) (repeat 1) !! 20 !! 20

