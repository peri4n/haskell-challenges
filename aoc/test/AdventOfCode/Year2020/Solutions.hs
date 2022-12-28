module AdventOfCode.Year2020.Solutions where

import AdventOfCode.Year2020.Day1 as Day1
import AdventOfCode.Year2020.Day2 as Day2
import AdventOfCode.Year2020.Day3 as Day3
import AdventOfCode.Year2020.Day4 as Day4
import Test.HUnit

problem1a = TestCase (Day1.solveA >>= assertEqual "Solution to problem 1a" 888331)

problem1b = TestCase (Day1.solveB >>= assertEqual "Solution to problem 1b" 130933530)

problem2a = TestCase (Day2.solveA >>= assertEqual "Solution to problem 2a" 607)

problem2b = TestCase (Day2.solveB >>= assertEqual "Solution to problem 2b" 321)

problem3a = TestCase (Day3.solveA >>= assertEqual "Solution to problem 3a" 153)

problem3b = TestCase (Day3.solveB >>= assertEqual "Solution to problem 3b" 2421944712)

problem4a = TestCase (Day4.solveA >>= assertEqual "Solution to problem 4a" 202)

problem4b = TestCase (Day4.solveB >>= assertEqual "Solution to problem 4b" 137)

solutions2020 :: Test
solutions2020 =
  TestLabel "Solutions of 2020" $
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
