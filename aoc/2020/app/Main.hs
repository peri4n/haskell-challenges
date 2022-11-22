module Main where

import System.Environment
import AdventOfCode.Day1 as Day1
import AdventOfCode.Day2 as Day2

main :: IO ()
main = do
  p <- getArgs
  solution <- case head p of
              "1a" -> Day1.solveA
              "1b" -> Day1.solveB
              "2a" -> Day2.solveA
              "2b" -> Day2.solveB
              _   -> return 0
  if solution == 0 then putStrLn $ "I have no solution to this problem " ++ head p else print solution
