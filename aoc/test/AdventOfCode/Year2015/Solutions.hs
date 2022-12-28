module AdventOfCode.Year2015.Solutions where

import AdventOfCode.Year2015.Day1 as Day1
import Test.HUnit

problem1a = TestCase (Day1.solveA >>= assertEqual "Solution to problem 1a" 280)

problem1b = TestCase (Day1.solveB >>= assertEqual "Solution to problem 1b" 1797)

solutions2015 :: Test
solutions2015 =
  TestLabel "Solutions of 2015" $
    TestList
      [ problem1a,
        problem1b
      ]
