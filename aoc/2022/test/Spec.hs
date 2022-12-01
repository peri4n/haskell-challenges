import AdventOfCode.Solutions
import Test.HUnit

main :: IO ()
main =
  runTestTTAndExit $
    TestList
      [ solutions
      ]
