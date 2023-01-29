module AdventOfCode.Year2022.Day8.Spec (day8Spec) where

import AdventOfCode.Year2022.Day8
import Data.HashSet as H
import Test.HUnit

day8Spec =
  TestList
    [ visibleTreesTest,
      countTreesTest,
      viewingDistanceTest
    ]

visibleTreesTest = TestCase $ do
  assertEqual "Correctly finds visible trees" 
    (visibles [1, 5, 7, 2, 5, 9, 15, 7, 20, 13, 9, 6, 7, 2])
    [True, True, True, False, False, True, True, False, True, False, False, False, False, False]

  assertEqual "Correctly finds visible trees" 
    (visibles [1, 4, 6, 5, 7, 7, 9, 8, 9, 1]) 
    [True, True, True, False, True, False, True, False, False, False]

  assertEqual "Correctly finds visible trees" 
    (visibles [2, 5, 5, 1, 2]) 
    [True, True, False, False, False]

  assertEqual "Correctly finds visible trees" 
    (visibles (reverse [2, 5, 5, 1, 2]))
    [True, False, True, False, False]

countTreesTest = TestCase $ do
  assertEqual
    "Correctly computs the example"
    (countVisible [[3, 0, 3, 7, 3], [2, 5, 5, 1, 2], [6, 5, 3, 3, 2], [3, 3, 5, 4, 9], [3, 5, 3, 9, 0]])
    21

viewingDistanceTest = TestCase $ do
  assertEqual
    "Correctly computes the viewing distance to the right of the first example line"
    [2, 1, 1, 1, 0]
    (viewingDistances [3, 0, 3, 7, 3])

  assertEqual
    "Correctly computes the viewing distance to the right of the second example line"
    [1, 1, 2, 1, 0]
    (viewingDistances [2, 5, 5, 1, 2])

  assertEqual
    "Correctly computes the viewing distance to the right of the second example line"
    [4, 3, 1, 1, 0]
    (viewingDistances [6, 5, 3, 3, 2])
