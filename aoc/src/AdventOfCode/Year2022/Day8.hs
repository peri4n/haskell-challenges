{-# LANGUAGE TupleSections #-}

module AdventOfCode.Year2022.Day8 where

import Data.Char (digitToInt)
import Data.HashSet as H
import Data.List as L
import Data.Tuple (snd, swap)
import Debug.Trace

-- solution
type Idx = Int

type Positions = HashSet (Int, Int)

treePositions :: Int -> [TreeHeight] -> Positions
treePositions idx xs = H.map (idx,) $ H.fromList (back ++ forth)
  where
    len = length xs
    back = visibleTrees xs
    forth = L.map (len + 1 -) $ visibleTrees (reverse xs)

visibleTrees :: [TreeHeight] -> [Idx]
visibleTrees heights = L.map snd (L.filter ((< 0) . fst) (zip incs [1 ..]))
  where
    maxs = -1 : scanl1 max heights
    incs = zipWith (-) maxs (tail maxs)

countVisible :: Input -> VisibleTrees
countVisible heights = H.size $ H.union rows columns
  where
    rows = L.foldl1' H.union $ zipWith treePositions [1 ..] heights
    columns = H.map swap $ L.foldl1' H.union $ zipWith treePositions [1 ..] (transpose heights)

solveA :: IO VisibleTrees
solveA = countVisible <$> treeHeights

solveB :: IO VisibleTrees
solveB = undefined

-- Load and parse data

type VisibleTrees = Int

type TreeHeight = Int

type Input = [[TreeHeight]]

treeHeights :: IO Input
treeHeights = do
  content <- readFile "data/2022/day8.txt"
  return $ L.map (L.map digitToInt) (lines content)
