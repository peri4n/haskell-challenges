module AdventOfCode.Year2022.Day5.Spec (day5Spec) where

import AdventOfCode.Year2022.Day5
import Data.IntMap.Strict (fromList)
import Data.List as L
import Test.HUnit
import Text.Megaparsec (parseMaybe)

day5Spec =
  TestList
    [ crateParser,
      crateLinesParser,
      stackLineParser,
      stateParser,
      programParser
    ]

crateParser = TestCase $ do
  assertEqual
    "Parse a single crate"
    (parseMaybe crate "[A]")
    (Just (Just 'A'))

  assertEqual
    "Parse a no crate"
    (parseMaybe crate "   ")
    (Just Nothing)

  assertEqual
    "Parse a single crate"
    (parseMaybe crate "[B]")
    (Just (Just 'B'))

crateLinesParser =
  TestCase $ do
    assertEqual
      "Parse a single crate line"
      (parseMaybe crateLine "[A] [B] [C]\n")
      ( Just
          [ Just 'A',
            Just 'B',
            Just 'C'
          ]
      )

    assertEqual
      "Parse a single crate line"
      (parseMaybe crateLine "[A]     [C]\n")
      ( Just
          [ Just 'A',
            Nothing,
            Just 'C'
          ]
      )

    assertEqual
      "Parse two crate lines"
      ( parseMaybe
          crateLines
          "    [A]    \n\
          \[B] [E] [D]\n\
          \[C] [F] [G]\n"
      )
      ( Just
          [ [Nothing, Just 'A', Nothing],
            [Just 'B', Just 'E', Just 'D'],
            [Just 'C', Just 'F', Just 'G']
          ]
      )

stackLineParser =
  TestCase $ do
    assertEqual
      "Parse the stack line"
      (parseMaybe stackLine " 1   2   3 \n")
      (Just [1, 2, 3])

stateParser =
  TestCase $ do
    assertEqual
      "Parse the example state"
      ( parseMaybe
          state
          "    [D]    \n\
          \[N] [C]    \n\
          \[Z] [M] [P]\n\
          \ 1   2   3 \n"
      )
      ( Just $
          fromList
            [ (1, ['N', 'Z']),
              (2, ['D', 'C', 'M']),
              (3, ['P'])
            ]
      )

moveParser =
  TestCase $ do
    assertEqual
      "Parse a move"
      ( parseMaybe
          move
          "move 1 from 2 to 3\n"
      )
      (Just (1, 2, 3))

programParser =
  TestCase $ do
    assertEqual
      "Parse the example program"
      ( parseMaybe
          program
          "    [D]    \n\
          \[N] [C]    \n\
          \[Z] [M] [P]\n\
          \ 1   2   3 \n\
          \\n\
          \move 1 from 3 to 2\n\
          \move 2 from 2 to 1\n"
      )
      ( Just $
          ( fromList
              [ (1, ['N', 'Z']),
                (2, ['D', 'C', 'M']),
                (3, ['P'])
              ],
            [(1, 3, 2), (2, 2, 1)]
          )
      )
