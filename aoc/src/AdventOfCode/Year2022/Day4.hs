module AdventOfCode.Year2022.Day4 where

import Data.List.Split (splitOn)
import Data.Functor ((<&>))

-- solutions
contains :: Section -> Section -> Bool
contains (start1, end1) (start2, end2) = start1 >= start2 && end1 <= end2

contain :: Section -> Section -> Bool
contain sec1 sec2 = sec1 `contains` sec2 || sec2 `contains` sec1

overlap :: Section -> Section -> Bool
overlap (start1, end1) (start2, end2) = end1 >= start2 && end2 >= start1

solveA :: IO Int
solveA = overlaps <&> filter (uncurry contain) <&> length

solveB :: IO Int
solveB = overlaps <&> filter (uncurry overlap) <&> length

-- parse file
type Section = (Int, Int)

type Input = (Section, Section)

overlaps :: IO [Input]
overlaps = do
  content <- readFile "data/2022/day4.txt"
  return $ map parseSections $ lines content

parseSections :: String -> Input
parseSections items = (section1, section2)
  where
    (section1 : section2 : _) = map parseSection $ splitOn "," items

parseSection :: String -> Section
parseSection section = (read start, read end)
  where
    (start : end : _) = splitOn "-" section
