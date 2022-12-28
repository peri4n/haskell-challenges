module AdventOfCode.Year2021.Day5 where

import Data.List as L
import Data.Map.Strict hiding (filter, map, mapMaybe)
import Data.Maybe (mapMaybe)
import Text.Regex.Applicative hiding (empty)
import Text.Regex.Applicative.Common (decimal)

type Coord = (Int, Int)

data Track = Track Coord Coord deriving (Show)

coord :: RE Char Coord
coord = (,) <$> decimal <* sym ',' <*> decimal

track :: RE Char Track
track = Track <$> coord <* string " -> " <*> coord

tracks :: IO [Track]
tracks = do
  ls <- lines <$> readFile "data/2021/day5.txt"
  return $ mapMaybe (=~ track) ls

isHV :: Track -> Bool
isHV (Track (x1, y1) (x2, y2)) = x1 == x2 || y1 == y2

countTrack :: Map Coord Int -> [Coord] -> Map Coord Int
countTrack = L.foldl' (\acc c -> insertWith (+) c 1 acc)

solveA :: IO Int
solveA = solve . filter isHV <$> tracks

segment :: Track -> [Coord]
segment (Track (x1, y1) (x2, y2))
  | x1 == x2 = [(x1, y) | y <- ys] -- Horizontal
  | y1 == y2 = [(x, y1) | x <- xs] -- Vertical
  | x1 + y1 == x2 + y2 = xs `zip` reverse ys -- Upwards
  | x1 - y1 == x2 - y2 = xs `zip` ys -- Downwards
  | otherwise = []
  where
    xs = [min x1 x2 .. max x1 x2]
    ys = [min y1 y2 .. max y1 y2]

solve :: [Track] -> Int
solve ts =
  let count = L.foldl' countTrack empty (map segment ts)
   in length $ filter (>= 2) (elems count)

solveB :: IO Int
solveB = solve <$> tracks
