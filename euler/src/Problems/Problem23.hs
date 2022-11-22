module Problems.Problem23 where

import Data.Set as S
import Data.List as L
import Numbers.Series

solve :: IO Int
solve = return solution

solution :: Int
solution = L.sum $ L.filter (`notMember` abundantSums) [1..28123]

abundantSums :: Set Int
abundantSums = S.fromList [a1 + a2 | a1 <- as, a2 <- as]
  where as = takeWhile (<28123) abundant
