module Problems.Problem87 where

import Numbers.Primes
import Data.Set as S
import Data.List as L

solve :: IO Int
solve = return solution

solution :: Int
solution = size $ S.fromList [s + c + q | s <- squares, c <- cubes, q <- quads, s + c + q < 50000000]

squares :: [Int]
squares = takeWhile (<50000000) $ L.map (^2) primes

cubes :: [Int]
cubes = takeWhile (<50000000) $ L.map (^3) primes

quads :: [Int]
quads = takeWhile (<50000000) $ L.map (^4) primes
