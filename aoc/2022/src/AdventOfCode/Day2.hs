module AdventOfCode.Day2 where

import Data.List.Split (splitOn)
import Data.Functor ((<&>))

-- Solution

type Score = Int

solveA :: IO Score
solveA = turns <&> map score <&> sum

score' :: Turn -> Score
score' (A, X) = 3
score' (A, Y) = 6
score' (A, Z) = 0
score' (B, X) = 0
score' (B, Y) = 3
score' (B, Z) = 6
score' (C, X) = 6 
score' (C, Y) = 0
score' (C, Z) = 3

score :: Turn -> Score
score t@(_, X) = score' t + 1
score t@(_, Y) = score' t + 2
score t@(_, Z) = score' t + 3

play :: Turn -> Turn
play (A, X) = (A, Z)
play (A, Y) = (A, X)
play (A, Z) = (A, Y)
play (B, X) = (B, X)
play (B, Y) = (B, Y)
play (B, Z) = (B, Z)
play (C, X) = (C, Y)
play (C, Y) = (C, Z)
play (C, Z) = (C, X)

solveB :: IO Score
solveB = turns <&> map play <&> map score <&> sum

-- Load and parse data

data MyMove = X | Y | Z
  deriving (Read, Show)

data OpponentsMove = A | B | C
  deriving (Read, Show)

type Turn = (OpponentsMove, MyMove)

type Input = [Turn]

turns :: IO Input
turns = do
  content <- readFile "data/day2.txt"
  return $ map parseTurn (lines content)

parseTurn :: String -> Turn
parseTurn line = (read his, read mine)
  where
    (his:mine:_) = splitOn " " line
