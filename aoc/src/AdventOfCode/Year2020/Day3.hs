module AdventOfCode.Year2020.Day3 where

type Forest = (Int, Int) -> Bool

forest :: IO (Forest, Int)
forest = do
  content <- readFile "data/2020/day3.txt"
  let height = length (lines content)
  return (field (lines content), height)

solveA :: IO Int
solveA = do
  (f, height) <- forest
  return $ walk f height (1,3)

solveB :: IO Int
solveB = do
  (f, height) <- forest

  let trees = map (walk f height) [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]

  return (product trees)

walk :: Forest -> Int -> (Int, Int) -> Int
walk forest h delta = length $ filter forest (slope h delta)

field :: [String] -> Forest
field ls = \(x, y) -> (ls !! x) !! (y `mod` width) == '#'
  where width = length (head ls)

slope :: Int -> (Int, Int) -> [(Int, Int)]
slope n (dx, dy)= takeWhile ((<n) . fst)  $ iterate (\(x, y) -> (x+dx, y+dy)) (0, 0)
