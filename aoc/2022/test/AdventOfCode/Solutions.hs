module AdventOfCode.Solutions where

import AdventOfCode.Day1 as Day1
import AdventOfCode.Day2 as Day2
import AdventOfCode.Day3 as Day3
import AdventOfCode.Day4 as Day4
import Test.HUnit

problem1a = TestCase (Day1.solveA >>= assertEqual "Solution to problem 1a" 65912)

problem1b = TestCase (Day1.solveB >>= assertEqual "Solution to problem 1b" 195625)

problem2a = TestCase (Day2.solveA >>= assertEqual "Solution to problem 2a" 14531)

problem2b = TestCase (Day2.solveB >>= assertEqual "Solution to problem 2b" 11258)

problem3a = TestCase (Day3.solveA >>= assertEqual "Solution to problem 3a" 7980)

problem3b = TestCase (Day3.solveB >>= assertEqual "Solution to problem 3b" 2881)

problem4a = TestCase (Day4.solveA >>= assertEqual "Solution to problem 4a" 513)

problem4b = TestCase (Day4.solveB >>= assertEqual "Solution to problem 4b" 878)

solutions :: Test
solutions =
  TestLabel "Solutions" $
    TestList
      [ problem1a,
        problem1b,
        problem2a,
        problem2b,
        problem3a,
        problem3b,
        problem4a,
        problem4b
      ]

