module AdventOfCode.Year2022.Solutions where

import AdventOfCode.Year2022.Day1 as Day1
import AdventOfCode.Year2022.Day2 as Day2
import AdventOfCode.Year2022.Day3 as Day3
import AdventOfCode.Year2022.Day4 as Day4
import AdventOfCode.Year2022.Day5 as Day5
import AdventOfCode.Year2022.Day6 as Day6
import Test.HUnit

problem1a = TestCase (Day1.solveA >>= assertEqual "Solution to problem 1a" 65912)

problem1b = TestCase (Day1.solveB >>= assertEqual "Solution to problem 1b" 195625)

problem2a = TestCase (Day2.solveA >>= assertEqual "Solution to problem 2a" 14531)

problem2b = TestCase (Day2.solveB >>= assertEqual "Solution to problem 2b" 11258)

problem3a = TestCase (Day3.solveA >>= assertEqual "Solution to problem 3a" 7980)

problem3b = TestCase (Day3.solveB >>= assertEqual "Solution to problem 3b" 2881)

problem4a = TestCase (Day4.solveA >>= assertEqual "Solution to problem 4a" 513)

problem4b = TestCase (Day4.solveB >>= assertEqual "Solution to problem 4b" 878)

problem5a = TestCase (Day5.solveA >>= assertEqual "Solution to problem 5a" "SBPQRSCDF")

problem5b = TestCase (Day5.solveB >>= assertEqual "Solution to problem 5b" "RGLVRCQSB")

problem6a = TestCase (Day6.solveA >>= assertEqual "Solution to problem 6a" 1707)

problem6b = TestCase (Day6.solveB >>= assertEqual "Solution to problem 6b" 3697)

solutions2022 :: Test
solutions2022 =
  TestLabel "Solutions of 2022" $
    TestList
      [ problem1a,
        problem1b,
        problem2a,
        problem2b,
        problem3a,
        problem3b,
        problem4a,
        problem4b,
        problem5a,
        problem5b,
        problem6a,
        problem6b
      ]

