module AdventOfCode.Year2022.Day8.Spec (day8Spec) where

import AdventOfCode.Year2022.Day8
import Data.HashSet as H
import Test.HUnit

day8Spec =
  TestList
    [ visibleTreesTest,
      treePositionsTest,
      countTreesTest
    ]

visibleTreesTest = TestCase $ do
  assertEqual "Correctly finds visible trees" (visibleTrees [1, 5, 7, 2, 5, 9, 15, 7, 20, 13, 9, 6, 7, 2]) [1, 2, 3, 6, 7, 9]
  assertEqual "Correctly finds visible trees" (visibleTrees [1, 4, 6, 5, 7, 7, 9, 8, 9, 1]) [1, 2, 3, 5, 7]
  assertEqual "Correctly finds visible trees" (visibleTrees [2, 5, 5, 1, 2]) [1, 2]
  assertEqual "Correctly finds visible trees" (visibleTrees (reverse [2, 5, 5, 1, 2])) [1, 3]

treePositionsTest = TestCase $ do
  assertEqual
    "Correctly finds visible back and forth"
    (treePositions 0 [1, 4, 6, 5, 7, 7, 9, 8, 9, 1])
    (H.fromList $ zip (repeat 0) [1, 2, 3, 5, 7, 9, 10])
  assertEqual "Correctly finds visible back and forth" 
    (treePositions 0 [2, 5, 5, 1, 2]) 
    (H.fromList $ zip (repeat 0) [1, 2, 3, 5])

countTreesTest = TestCase $ do
  assertEqual
    "Correctly computs the example"
    (countVisible [[3, 0, 3, 7, 3], [2, 5, 5, 1, 2], [6, 5, 3, 3, 2], [3, 3, 5, 4, 9], [3, 5, 3, 9, 0]])
    21
