module AdventOfCode.Solutions where

import AdventOfCode.Day1 as Day1
import Test.HUnit

problem1a = TestCase (Day1.solveA >>= assertEqual "Solution to problem 1a" 65912)

problem1b = TestCase (Day1.solveB >>= assertEqual "Solution to problem 1b" 195625)

solutions :: Test
solutions =
  TestLabel "Solutions" $
    TestList
      [ problem1a,
        problem1b
      ]
