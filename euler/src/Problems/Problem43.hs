module Problems.Problem43 where

import Numbers.Core (divides, toDigits)
import Numbers.Series (pandigital)

solve :: IO Int
solve = return solution

solution :: Int
solution = sum $ [x | x <- pandigital 9, p1 x, p2 x, p3 x, p4 x, p5 x, p6 x, p7 x]

pn :: Int -> Int -> Int -> Bool
pn ix d x = (a * 100 + b * 10 + c) `divides` d
  where xs = toDigits x
        a = xs !! ix 
        b = xs !! (ix + 1)
        c = xs !! (ix + 2)

p1 :: Int -> Bool
p1 = pn 1 2

p2 :: Int -> Bool
p2 = pn 2 3

p3 :: Int -> Bool
p3 = pn 3 5

p4 :: Int -> Bool
p4 = pn 4 7

p5 :: Int -> Bool
p5 = pn 5 11 

p6 :: Int -> Bool
p6 = pn 6 13

p7 :: Int -> Bool
p7 = pn 7 17
