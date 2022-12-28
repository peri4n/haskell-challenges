module AdventOfCode.Year2021.Day8 where

import Data.Char
import Data.List as L
import Data.Map as M
import Data.Maybe as MB
import Text.Regex.Applicative
import Text.Regex.Applicative.Common

data Encoded = Encoded [String] [String] deriving (Show)

data Decoded = Decoded [Int] [Int] deriving (Show)

str :: RE Char String
str = many $ psym isAlpha

codesRE :: RE Char Encoded
codesRE = Encoded <$> left <* sep <*> right
  where
    left = many (str <* sym ' ')
    sep = sym '|'
    right = many (sym ' ' *> str)

codes :: IO [Encoded]
codes = do
  ls <- lines <$> readFile "data/2021/day8.txt"
  return $ MB.mapMaybe (=~ codesRE) ls

decodes :: IO [Decoded]
decodes = L.map decode <$> codes

solveA :: IO Int
solveA = do
  dec <- decodes
  let out = L.map (\(Decoded _ out) -> length $ L.filter (`elem` [1,4,7,8]) out) dec
  return $ sum out

solveB :: IO Int
solveB = do
  dec <- decodes
  let out = L.map (\(Decoded _ out) -> head out * 1000 + out !! 1 * 100 + out !! 2 * 10 + out !! 3) dec
  return $ sum out

type Candidates = Map Char String

type Mapping = Map Char Char

type Rule = Candidates -> Candidates

decode :: Encoded -> Decoded
decode (Encoded ws ds) = Decoded (fromMaybe [] (mapM dec ws)) (fromMaybe [] (mapM dec ds))
  where
    dec str = (`M.lookup` decoder) (translate (mapping ws) str)

mapping :: [String] -> Mapping
mapping ws = invert $ fromMaybe M.empty $ find unique $ iterate (allRules ws) newPuzzle
  where
    unique = all ((== 1) . length)
    newPuzzle = fromList $ L.map (\s -> (s, ['a' .. 'g'])) ['a' .. 'g']

invert :: Candidates -> Mapping
invert = M.foldlWithKey' (\acc k v -> M.insert (head v) k acc) M.empty

translate :: Mapping -> String -> String
translate m str = sort $ fromMaybe "" $ mapM (`M.lookup` m) str

allRules :: [String] -> Rule
allRules ws = foldl1' (.) $ reverse [refineOne ws, refineFour ws, refineSeven ws, refineEight ws, refineDG ws, refineBFG ws, refineUnique]

decoder :: Map String Int
decoder =
  fromList
    [ ("abcefg", 0),
      ("cf", 1),
      ("acdeg", 2),
      ("acdfg", 3),
      ("bcdf", 4),
      ("abdfg", 5),
      ("abdefg", 6),
      ("acf", 7),
      ("abcdefg", 8),
      ("abcdfg", 9)
    ]

refine :: Char -> String -> Rule
refine = M.insertWith L.intersect

fetchEnc :: [String] -> Int -> String
fetchEnc ws l = head $ fetchEncs ws l

fetchEncs :: [String] -> Int -> [String]
fetchEncs ws l = L.filter ((== l) . length) ws

refineOne :: [String] -> Rule
refineOne ws = refine 'c' one . refine 'f' one
  where
    one = fetchEnc ws 2

refineFour :: [String] -> Rule
refineFour ws = refine 'b' bd . refine 'd' bd
  where
    four = fetchEnc ws 4
    one = fetchEnc ws 2
    bd = four L.\\ one

refineSeven :: [String] -> Rule
refineSeven ws = refine 'a' a
  where
    a = seven L.\\ one
    seven = fetchEnc ws 3
    one = fetchEnc ws 2

refineEight :: [String] -> Rule
refineEight ws = refine 'e' eg . refine 'g' eg
  where
    eg = (eight L.\\ seven) L.\\ four
    eight = fetchEnc ws 7
    seven = fetchEnc ws 3
    four = fetchEnc ws 4

refineDG :: [String] -> Rule
refineDG ws c =
  let twoThreeFive = fetchEncs ws 5
      ttf = foldl1' L.intersect twoThreeFive
      dg = ttf L.\\ fromMaybe "" (M.lookup 'a' c)
   in (refine 'd' dg . refine 'g' dg) c

refineBFG :: [String] -> Rule
refineBFG ws c =
  let zeroSixNine = fetchEncs ws 6
      zsn = foldl1' L.intersect zeroSixNine
      bfg = zsn L.\\ fromMaybe "" (M.lookup 'a' c)
   in (refine 'b' bfg . refine 'f' bfg . refine 'g' bfg) c

refineUnique :: Rule
refineUnique c = M.map upd c
  where
    upd cs = if length cs > 1 then L.filter (\k -> [k] `notElem` values) cs else cs
    values = elems c
