module AdventOfCode.Common.Points where

type Point = (Int, Int)

type Segment = (Point, Point)

-- | Checks if two points are on the same horizontal line
horizontalEq :: Point -> Point -> Bool
horizontalEq (_, y1) (_, y2) = y1 == y2

-- | Checks if two points are on the same vertical line
verticalEq :: Point -> Point -> Bool
verticalEq (x1, _) (x2, _) = x1 == x2

-- | Checks if two points are on a diagonal
diagEq :: Point -> Point -> Bool
diagEq p1 p2 = upDiagEq p1 p2 || downDiagEq p1 p2

-- | Checks if two points are on an upwards diagonal
upDiagEq :: Point -> Point -> Bool
upDiagEq (x1, y1) (x2, y2) = (x2 - x1) == (y2 - y1)

-- | Checks if two points are on a downwards diagonal
downDiagEq :: Point -> Point -> Bool
downDiagEq (x1, y1) (x2, y2) = (x2 - x1) == (y1 - y2)

segment :: Segment -> [Point]
segment (p1@(x1, y1), p2@(x2, y2))
  | p1 `horizontalEq` p2 = [(x, y1) | x <- xs] -- Horizontal
  | p1 `verticalEq` p2 = [(x1, y) | y <- ys] -- Vertical
  | p1 `upDiagEq` p2 = xs `zip` ys
  | p1 `downDiagEq` p2 = xs `zip` reverse ys
  | otherwise = []
  where
    xs = [min x1 x2 .. max x1 x2]
    ys = [min y1 y2 .. max y1 y2]
