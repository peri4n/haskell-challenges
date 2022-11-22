module AdventOfCode.Day8Spec (day8Spec) where

import AdventOfCode.Day8
import Data.List as L
import Data.Map
import Data.Maybe
import Test.HUnit

day8Spec =
  TestList
    [ testExample1,
      testExamples
    ]

candidates = ['a' .. 'g']

example1 = ["acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", "cefabd", "cdfgeb", "eafb", "cagedb", "ab"]

example2 = ["begfcd", "fabg", "aecgbdf", "cefagb", "edgcba", "eacbf", "efgbc", "bca", "ab", "decfa"]

example3 = ["cfdegb", "bdefca", "ac", "becdf", "cefagd", "bfac", "cfegbad", "aegbd", "cad", "beacd"]

isValid :: Mapping -> [String] -> Bool
isValid m ws = all (`member` decoder) segs
  where
    segs = L.map (translate m) ws

testExample1 =
  TestCase $ do
    let solution = mapping example1
    assertEqual
      "After refining 'c' should have only one candidates"
      ( fromList
          [ ('d', 'a'),
            ('e', 'b'),
            ('a', 'c'),
            ('f', 'd'),
            ('g', 'e'),
            ('b', 'f'),
            ('c', 'g')
          ]
      )
      solution

testExamples =
  TestCase $ do
    assertBool "Solution to example 1 should be valid" (isValid (mapping example1) example1)
    assertBool "Solution to example 2 should be valid" (isValid (mapping example2) example2)
    assertBool "Solution to example 3 should be valid" (isValid (mapping example3) example3)
