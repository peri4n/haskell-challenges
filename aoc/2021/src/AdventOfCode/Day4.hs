module AdventOfCode.Day4 where

import Data.Char
import Data.Maybe
import Data.List.Split
import Data.List
import Data.Functor

solveA :: IO Int
solveA = do
  (draws, boards) <- setup
  return $ playA draws boards

solveB :: IO Int
solveB = do
  (draws, boards) <- setup
  return $ playB draws boards

type Bingo = [Cell]

newBingo :: [Int] -> Bingo
newBingo = map (Cell False)

bingoIndizes =
  [ -- rows
    [0, 1, 2, 3, 4],
    [5, 6, 7, 8, 9],
    [10, 11, 12, 13, 14],
    [15, 16, 17, 18, 19],
    [20, 21, 22, 23, 24],
    -- colums
    [0, 5, 10, 15, 20],
    [1, 6, 11, 16, 21],
    [2, 7, 12, 17, 22],
    [3, 8, 13, 18, 23],
    [4, 9, 14, 19, 24]
  ]

hasBingo :: Bingo -> Bool
hasBingo b = any (allMarked b) bingoIndizes
  where
    allMarked b = all (\i -> marked (b !! i))

unmarkedSum :: Bingo -> Int
unmarkedSum b = sum $ map f b
  where f (Cell m v) = if m then 0 else v

draw :: Int -> [Bingo] -> [Bingo]
draw d = map (map (mark d))

playA :: [Int] -> [Bingo] -> Int
playA [] bs = 0
playA (d:ds) bs = if bingo then d * b else playA ds nb
  where nb = draw d bs
        bingo = any hasBingo nb
        b = maybe 0 unmarkedSum (find hasBingo nb)

playB :: [Int] -> [Bingo] -> Int
playB [] bs = 0
playB (d:ds) bs = if bingo then d * b else playB ds notBingos
  where nb = draw d bs
        notBingos = filter (not . hasBingo) nb
        bingo = null notBingos
        b = unmarkedSum (head nb)

data Cell = Cell
  { marked :: Bool,
    value :: Int
  }
  deriving (Show)

mark :: Int -> Cell -> Cell
mark x (Cell m v) = if v == x then Cell True v else Cell m v

setup :: IO ([Int], [Bingo])
setup = do
  content <- readFile "data/day4.txt"
  let (l : ls) = splitOn "\n\n" content
  let draws = parseDraws l
  let boards = parseBoards ls
  return (draws, boards)

parseDraws :: String -> [Int]
parseDraws = map read . splitOn [',']

parseBoards :: [String] -> [Bingo]
parseBoards = map parseBoard

parseBoard :: String -> Bingo
parseBoard ls = newBingo cells
  where
    cells = map read (words ls)
