module Numbers.Primes ( primeFactors
                      , isPrime
                      , primes
                      , divisors
                      , nDivisors
                      ) where

import Data.List (group, find)
import Data.IntMap (assocs, fromList, IntMap)
import Numbers.Core

primeFactors :: Int -> IntMap Int
primeFactors x = fromList $ count $ primeFactors' x []

count :: (Eq a ) => [a] -> [(a, Int)]
count xs = map (\x -> (head x, length x)) (group xs)

primeFactors' :: Int -> [Int] -> [Int]
primeFactors' x acc =
  let divisor = find (\i -> x `divides` i) (2:[3, 5..x])
   in case divisor of
        Just d -> primeFactors' (x `quot` d) (d:acc)
        Nothing -> acc

isPrime :: Int -> Bool
isPrime x 
  | x <= 1 = False
  | otherwise = not $ any (divides x) [2,3..(floor $ sqrt (fromIntegral x))]

primes :: [Int]
primes = filter isPrime (2:[3,5..])

nDivisors :: Int -> Int
nDivisors n = foldl mult 1 (primeFactors n)
  where mult a b = a * (b+1)

divisors :: Int -> [Int]
divisors n = map product $ mapM (\(p,k) -> take (k+1) (iterate (*p) 1)) pf
  where pf = assocs (primeFactors n)
