module Problems.Problem4 where

import Data.List
import Data.Maybe

-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.

solve :: IO Int
solve = return solution

solution :: Int
solution = maximum allPalindromes

isPalindrome :: Int -> Bool
isPalindrome x = str == reverse str
        where str = show x

dec3DigitNumbers :: [Int]
dec3DigitNumbers = [999,998..100]

allPalindromes :: [Int]
allPalindromes = [ i * j | i <- dec3DigitNumbers, j <- dec3DigitNumbers, isPalindrome (i * j)]

