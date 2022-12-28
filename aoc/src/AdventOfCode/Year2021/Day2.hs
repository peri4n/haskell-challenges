module AdventOfCode.Year2021.Day2 where

import qualified Data.Char as Char
import Data.List

commands :: IO [Command]
commands = do
  content <- readFile "data/2021/day2.txt"
  return $ map read (lines content)

solveA :: IO Int
solveA = do
  ns <- commands
  let (x, d) = foldl' moveA (0, 0) ns
  return $ x * d

moveA :: (Int, Int) -> Command -> (Int, Int)
moveA (x, d) (Cmd Forward k) = (x + k, d)
moveA (x, d) (Cmd Up k) = (x, d - k)
moveA (x, d) (Cmd Down k) = (x, d + k)

solveB :: IO Int
solveB = do
  ns <- commands
  let (x, d, a) = foldl' moveB (0, 0, 0) ns
  return $ x * d

moveB :: (Int, Int, Int) -> Command -> (Int, Int, Int)
moveB (x, d, a) (Cmd Forward k) = (x + k, d + a * k, a)
moveB (x, d, a) (Cmd Up k) = (x, d, a - k)
moveB (x, d, a) (Cmd Down k) = (x, d, a + k)

data Direction = Forward | Up | Down
  deriving (Show)

instance Read Direction where
  readsPrec _ str =
    case fmap Char.toLower str of
      "forward" -> [(Forward, "")]
      "up" -> [(Up, "")]
      "down" -> [(Down, "")]
      _ -> []

data Command = Cmd Direction Int
  deriving (Show)

instance Read Command where
  readsPrec _ str =
    let (direction, count) = split ' ' str
     in [(Cmd (read direction) (read count), "")]

split :: (Eq a) => a -> [a] -> ([a], [a])
split s xs = (direction, tail count)
  where
    (direction, count) = span (/= s) xs
