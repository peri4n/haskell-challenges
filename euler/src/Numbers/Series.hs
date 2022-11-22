module Numbers.Series
        ( collatz
        , triplets
        , pyTriplets
        , abundant
        , triangles
        , pandigital
        , pandigital1
        ) where

import Numbers.Primes (divisors)
import Numbers.Core
import Numbers.Combinatorics (permute)

collatz :: Int -> Int
collatz n
  | even n =  1 + collatz (quot n 2)
  | n == 1 = 1
  | otherwise = 1 + collatz (3 * n + 1)

triplets :: [(Int, Int, Int)]
triplets = [(a,b,c) | c <- [1..1000], b <- [1..(c-1)], a <- [1..(b-1)]]

isPythagoraen :: (Int, Int,Int) -> Bool
isPythagoraen (a,b,c) = (a*a) + (b*b) == (c*c)

pyTriplets :: [(Int, Int, Int)]
pyTriplets = filter isPythagoraen triplets

isAbundant :: Int -> Bool
isAbundant n = n < (sum (divisors n) - n)

abundant :: [Int]
abundant = filter isAbundant [12..]

triangles :: [Int]
triangles = map (\n -> n * (n + 1) `div` 2) [1..]

fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d

pandigital :: Int -> [Int]
pandigital n = filter (> 10^n) $  map fromDigits (permute [0..n])

pandigital1 :: Int -> [Int]
pandigital1 n = map fromDigits (permute [1..n])
