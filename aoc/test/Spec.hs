import AdventOfCode.Year2022.Day5.Spec
import AdventOfCode.Year2022.Solutions
import AdventOfCode.Year2021.Solutions
import AdventOfCode.Year2020.Solutions
import AdventOfCode.Year2015.Solutions
import Test.HUnit

main :: IO ()
main =
  runTestTTAndExit $
    TestList
      [ solutions2022,
        solutions2021,
        solutions2020,
        solutions2015,
        day5Spec
      ]
