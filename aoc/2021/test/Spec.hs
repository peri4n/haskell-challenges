import AdventOfCode.Solutions
import Test.HUnit
import AdventOfCode.Day8Spec

main :: IO ()
main =
  runTestTTAndExit $
    TestList
      [ solutions,
        day8Spec
      ]
