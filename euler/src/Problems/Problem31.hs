module Problems.Problem31 where

solve :: IO Int
solve = return solution

solution :: Int
solution = change 200 (length coins - 1)

coins :: [Int]
coins = [200, 100, 50, 20, 10, 5, 2, 1]

change :: Int -> Int -> Int
change n m
  | n < 0 || m < 0 = 0
  | n == 0 = 1
  | otherwise = change n (m-1) + change (n - (coins !! m)) m
