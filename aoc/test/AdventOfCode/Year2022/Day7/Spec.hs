module AdventOfCode.Year2022.Day7.Spec (day7Spec) where

import AdventOfCode.Year2022.Day7.Parser (Command (Cd), parseCd)
import Data.IntMap.Strict (fromList)
import Data.List as L
import Test.HUnit
import Text.Megaparsec (parseMaybe)

day7Spec =
  TestList
    [ cdParser
    ]

cdParser = TestCase $ do
  assertEqual
    "Parse a 'cd' line"
    (parseMaybe parseCd "cd foo\n")
    (Just (Cd "foo"))
