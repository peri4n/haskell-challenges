module AdventOfCode.Year2021.Day5 where

import AdventOfCode.Common.Points
import Data.List as L
import Data.HashMap.Strict as M hiding (mapMaybe)
import Data.Maybe (mapMaybe)
import Text.Regex.Applicative hiding (empty)
import Text.Regex.Applicative.Common (decimal)

coord :: RE Char Point
coord = (,) <$> decimal <* sym ',' <*> decimal

track :: RE Char Segment
track = (,) <$> coord <* string " -> " <*> coord

tracks :: IO [Segment]
tracks = do
  ls <- lines <$> readFile "data/2021/day5.txt"
  return $ mapMaybe (=~ track) ls

isHV :: Segment -> Bool
isHV (p1, p2) = p1 `horizontalEq` p2 || p1 `verticalEq` p2

countSegment :: HashMap Point Int -> [Point] -> HashMap Point Int
countSegment = L.foldl' (\acc c -> M.insertWith (+) c 1 acc)

solveA :: IO Int
solveA = solve . L.filter isHV <$> tracks

solve :: [Segment] -> Int
solve ts =
  let count = L.foldl' countSegment empty (L.map segment ts)
   in length $ L.filter (>= 2) (M.elems count)

solveB :: IO Int
solveB = solve <$> tracks
