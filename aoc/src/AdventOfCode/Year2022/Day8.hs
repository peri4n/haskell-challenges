module AdventOfCode.Year2022.Day8 where

import Data.Char (digitToInt)
import Data.List as L
import Data.Tuple (snd, swap)
import Debug.Trace

-- solution

-- part A
agg :: ([TreeHeight] -> [a]) -> (a -> a -> a) -> Input -> [a]
agg func combiner heights = zipWith combiner (L.concat rows) (L.concat columns)
  where
    rows = L.map bothFunc heights
    columns = transpose $ L.map bothFunc (transpose heights)
    bothFunc xs = zipWith combiner back forth
      where
        back = func xs
        forth = reverse $ func (reverse xs)

visibles :: [TreeHeight] -> [Bool]
visibles heights = L.map (< 0) incs
  where
    maxs = -1 : scanl1 max heights
    incs = zipWith (-) maxs (tail maxs)

countVisible :: Input -> VisibleTrees
countVisible = count (== True) . agg visibles (||)
  where
    count p = L.length . L.filter p

solveA :: IO VisibleTrees
solveA = countVisible <$> treeHeights

-- part B
subs :: [a] -> [[a]]
subs = reverse . drop 1 . reverse .tails

view :: [Int] -> Int
view [] = 0
view [x] = 0
view (x : xs)
  | L.null rest = visibleTrees
  | head rest == x = visibleTrees + 1
  | otherwise = max 1 visibleTrees
  where
      (visible, rest) = span (< x) xs
      visibleTrees = length visible

viewingDistances :: [TreeHeight] -> [Int]
viewingDistances hs = L.map view (subs hs)

scenicViews :: Input -> ScenicViews
scenicViews = maximum . agg viewingDistances (*)

solveB :: IO VisibleTrees
solveB = scenicViews <$> treeHeights

-- Load and parse data

type VisibleTrees = Int

type ScenicViews = Int

type TreeHeight = Int

type Input = [[TreeHeight]]

readGrid :: String -> [[Int]]
readGrid = L.map (L.map digitToInt) . lines

treeHeights :: IO Input
treeHeights = readGrid <$> readFile "data/2022/day8.txt"
