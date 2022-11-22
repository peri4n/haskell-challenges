module Problems.Problem17 where

solve :: IO Int
solve = return solution

solution :: Int
solution = 11 + sum (map letterCount [1..999])

twoDigits :: Int -> Int
twoDigits 0 = 0
twoDigits 1 = 3 -- one
twoDigits 2 = 3 -- two
twoDigits 3 = 5 -- three
twoDigits 4 = 4 -- four
twoDigits 5 = 4 -- five
twoDigits 6 = 3 -- six
twoDigits 7 = 5 -- seven
twoDigits 8 = 5 -- eight
twoDigits 9 = 4 -- nine
twoDigits 10 = 3 -- ten
twoDigits 11 = 6 -- eleven
twoDigits 12 = 6 -- twelve
twoDigits 13 = 8 -- thirteen
twoDigits 14 = 8 -- fourteen
twoDigits 15 = 7 -- fifteen
twoDigits 16 = 7 -- sixteen
twoDigits 17 = 9 -- seventeen
twoDigits 18 = 8 -- eighteen
twoDigits 19 = 8 -- nineteen
twoDigits 20 = 6 -- twenty
twoDigits 30 = 6 -- thirty
twoDigits 40 = 5 -- forty
twoDigits 50 = 5 -- fifty
twoDigits 60 = 5 -- sixty
twoDigits 70 = 7 -- seventy
twoDigits 80 = 6 -- eighty
twoDigits 90 = 6 -- ninety
twoDigits x = twoDigits (x - (x `mod` 10)) + twoDigits (x `mod` 10)

hundreds :: Int -> Int
hundreds x
  | x < 100 = 0
  | x `mod` 100 == 0 = twoDigits (x `div` 100) + 7-- hundred
  | otherwise = twoDigits (x `div` 100) + 10 -- hundred and

letterCount :: Int -> Int
letterCount x = h + t
  where h = hundreds x
        t = twoDigits (x `rem` 100)
