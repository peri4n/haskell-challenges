module Numbers.Fibonacci
    ( fibonacci
    ) where

fibonacci :: [Int]
fibonacci = fibonacci' 0 1

fibonacci' :: Int -> Int -> [Int]
fibonacci' x y = (x+y) : fibonacci' y (x+y)
