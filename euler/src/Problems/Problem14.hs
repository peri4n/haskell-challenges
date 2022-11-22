module Problems.Problem14 where

import Numbers.Series
import Data.List
import Data.Function (on)

-- Which starting number, under one million, produces the longest chain?

solve :: IO Int
solve = return solution

solution :: Int
solution = fst $ maximumBy (compare `on` snd) $ map (\x -> (x, collatz x)) [1..1000000]
