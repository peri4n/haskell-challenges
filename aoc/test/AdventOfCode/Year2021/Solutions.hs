module AdventOfCode.Year2021.Solutions where

import AdventOfCode.Year2021.Day1 as Day1
import AdventOfCode.Year2021.Day2 as Day2
import AdventOfCode.Year2021.Day4 as Day4
import AdventOfCode.Year2021.Day5 as Day5
import AdventOfCode.Year2021.Day6 as Day6
import AdventOfCode.Year2021.Day7 as Day7
import AdventOfCode.Year2021.Day8 as Day8
import AdventOfCode.Year2021.Day9 as Day9
import Test.HUnit

problem1a = TestCase (Day1.solveA >>= assertEqual "Solution to problem 1a" 1711)

problem1b = TestCase (Day1.solveB >>= assertEqual "Solution to problem 1b" 1743)

problem2a = TestCase (Day2.solveA >>= assertEqual "Solution to problem 2a" 1660158)

problem2b = TestCase (Day2.solveB >>= assertEqual "Solution to problem 2b" 1604592846)

problem4a = TestCase (Day4.solveA >>= assertEqual "Solution to problem 4a" 58374)

problem4b = TestCase (Day4.solveB >>= assertEqual "Solution to problem 4b" 11377)

problem5a = TestCase (Day5.solveA >>= assertEqual "Solution to problem 5a" 7297)

problem5b = TestCase (Day5.solveB >>= assertEqual "Solution to problem 5b" 21038)

problem6a = TestCase (Day6.solveA >>= assertEqual "Solution to problem 6a" 396210)

problem6b = TestCase (Day6.solveB >>= assertEqual "Solution to problem 6b" 1770823541496)

problem7a = TestCase (Day7.solveA >>= assertEqual "Solution to problem 7a" 328187)

problem7b = TestCase (Day7.solveB >>= assertEqual "Solution to problem 7b" 91257582)

problem8a = TestCase (Day8.solveA >>= assertEqual "Solution to problem 8a" 367)

problem8b = TestCase (Day8.solveB >>= assertEqual "Solution to problem 8b" 974512)

problem9a = TestCase (Day9.solveA >>= assertEqual "Solution to problem 9a" 594)

solutions2021 :: Test
solutions2021 =
  TestLabel "Solutions of 2021" $
    TestList
      [ problem1a,
        problem1b,
        problem2a,
        problem2b,
        problem4a,
        problem4b,
        problem5a,
        problem5b,
        problem6a,
        problem6b,
        problem7a,
        problem7b,
        problem8a,
        problem8b,
        problem9a
      ]
