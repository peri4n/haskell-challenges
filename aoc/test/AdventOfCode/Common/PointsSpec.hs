module AdventOfCode.Common.PointsSpec where

import AdventOfCode.Common.Points
import Test.HUnit

pointsSpec =
  TestList
    [ horizontalCheck,
      verticalCheck,
      diagCheck,
      segmentCheck
    ]

horizontalCheck = TestCase $ do
  assertBool "Checks if points are on the same horizon" $ (1, 1) `horizontalEq` (8, 1)
  assertBool "Checks if points are on the same horizon" $ not $ (1, 1) `horizontalEq` (8, 2)

verticalCheck = TestCase $ do
  assertBool "Checks if points are on the same vertical" $ (1, 1) `verticalEq` (1, 8)
  assertBool "Checks if points are on the same vertical" $ not $ (1, 1) `verticalEq` (2, 8)

diagCheck = TestCase $ do
  assertBool "Checks if points are on the same upwards diagonal" $ (1, 1) `diagEq` (3, 3)
  assertBool "Checks if points are on the same upwards diagonal" $ (2, 4) `diagEq` (4, 6)
  assertBool "Checks if points are on the same upwards diagonal" $ not $ (1, 1) `diagEq` (4, 3)
  assertBool "Checks if points are on the same downwards diagonal" $ (3, 3) `diagEq` (4, 2)
  assertBool "Checks if points are on the same downwards diagonal" $ (3, 3) `diagEq` (5, 1)
  assertBool "Checks if points are on the same downwards diagonal" $ not $ (3, 3) `diagEq` (5, 2)

segmentCheck = TestCase $ do
  assertEqual
    "Checks the points on the segment between points"
    (segment ((1, 1), (3, 3)))
    [(1, 1), (2, 2), (3, 3)]

  assertEqual
    "Checks the points on the segment between points"
    (segment ((3, 3), (5, 1)))
    [(3, 3), (4, 2), (5, 1)]
