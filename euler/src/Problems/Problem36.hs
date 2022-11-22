module Problems.Problem36 where

import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)

solve :: IO Int
solve = return solution

solution :: Int
solution = sum $ [x | x <- [1..1000000], isPalindrome x, isBinPalindrome x]

showBin :: Int -> String
showBin x = showIntAtBase 2 intToDigit x ""

isBinPalindrome :: Int -> Bool
isBinPalindrome =  isPalindrome . showBin

isPalindrome :: (Show a) => a -> Bool
isPalindrome x = str == reverse str
        where str = show x
