module Problems.Problem6 where

-- The sum of the squares of the first ten natural numbers is 385
-- The square of the sum of the first ten natural numbers is 3025
--
-- Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 - 385 = 2640.
-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

solve :: IO Int
solve = return solution

solution :: Int
solution = squareOfSum - sumOfSquares

numbers :: [Int]
numbers = [1..100]

sumOfSquares :: Int
sumOfSquares = sum $ map (\x -> x * x) numbers

squareOfSum :: Int
squareOfSum =
        let s = sum numbers
         in s * s
