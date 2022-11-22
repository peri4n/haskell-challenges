module AdventOfCode.Day9 where

import Data.List as L
import Data.Maybe as MB
import Data.Map as M
import Data.Char as C

type Coord = (Int, Int)

type Grid a = Map Coord a

grid :: IO (Grid Int)
grid = do
  ls <- lines <$> readFile "data/day9.txt"
  return $ M.map digitToInt (mkGrid ls)

solveA :: IO Int
solveA = do
  g <- grid
  let lowpoints = L.filter (isLowerPoint g) (M.keys g)
  return $ sum $ MB.mapMaybe (riskLevel g) lowpoints

riskLevel :: Grid Int -> Coord -> Maybe Int
riskLevel g c = (+1) <$> level
  where level = M.lookup c g

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [1..]

mkGrid :: [[a]] -> Grid a
mkGrid xs = M.fromList $ L.concatMap (uncurry createPoints) ixs
  where ixs = zipWithIndex xs
        createPoints i ps = L.map (\(ind, p) -> ((ind, i), p)) (zipWithIndex ps)

neighbours :: Coord -> [Coord]
neighbours (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

isLowerPoint :: Grid Int -> Coord -> Bool
isLowerPoint grid c = all (>value) nValues
  where n = neighbours c
        value = fromMaybe 0 $ M.lookup c grid
        nValues = MB.mapMaybe (`M.lookup` grid) n
