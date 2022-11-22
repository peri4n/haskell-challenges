module Numbers.Core where

divides :: Int -> Int -> Bool
divides x d = (x `mod` d) == 0

toDigits :: Integral x => x -> [x]
toDigits 0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]
