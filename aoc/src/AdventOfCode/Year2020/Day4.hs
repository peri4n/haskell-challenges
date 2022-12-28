module AdventOfCode.Year2020.Day4 where

import Data.Char (isDigit)
import Data.List as L
import Data.Map as M hiding (split)

credentials :: IO [String]
credentials = fmap (join ' ') . splitOn "" . lines <$> readFile "data/2020/day4.txt"

parse :: String -> Map String String
parse str = fromList attr
  where
    pairs = splitOn ' ' str
    attr = L.map (split ':') pairs

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn x xs = cred : splitOn x (L.drop 1 rest)
  where
    (cred, rest) = span (/= x) xs

split :: (Eq a) => a -> [a] -> ([a], [a])
split x xs = (co, L.drop 1 rest)
  where
    (co, rest) = span (/= x) xs

join :: Char -> [String] -> String
join sep = foldl1 (\a b -> a ++ [sep] ++ b)

solveA :: IO Int
solveA = do
  creds <- credentials
  let p = L.map parse creds
  return $ length $ L.filter validA p

validA :: Map String String -> Bool
validA m =
  member "byr" m
    && member "iyr" m
    && member "eyr" m
    && member "hgt" m
    && member "hcl" m
    && member "ecl" m
    && member "pid" m

solveB :: IO Int
solveB = do
  creds <- credentials
  let p = L.map parse creds
  return $ length $ L.filter validB p

validB :: Map String String -> Bool
validB m =
  birthYear m
    && issueYear m
    && expirationYear m
    && height m
    && hairColor m
    && eyeColor m
    && passport m

birthYear :: Map String String -> Bool
birthYear m = orFalse v
  where
    year = read <$> M.lookup "byr" m
    v = (\y -> (y >= 1920) && (y <= 2002)) <$> year

issueYear :: Map String String -> Bool
issueYear m = orFalse v
  where
    year = read <$> M.lookup "iyr" m
    v = (\y -> (y >= 2010) && (y <= 2020)) <$> year

expirationYear :: Map String String -> Bool
expirationYear m = orFalse v
  where
    year = read <$> M.lookup "eyr" m
    v = (\y -> (y >= 2020) && (y <= 2030)) <$> year

height :: Map String String -> Bool
height m = validateMeasure pair
  where
    year = M.lookup "hgt" m
    pair = span isDigit <$> year

validateMeasure :: Maybe (String, String) -> Bool
validateMeasure (Just (x, "cm")) = let k = read x in (150 <= k) && (k <= 193)
validateMeasure (Just (x, "in")) = let k = read x in (59 <= k) && (k <= 76)
validateMeasure _ = False

hairColor :: Map String String -> Bool
hairColor m = orFalse hc && orFalse areHex && orFalse len
  where
    hair = M.lookup "hcl" m
    hc = (== '#') . head <$> hair
    rgb = tail <$> hair
    len = (==6) . length <$> rgb
    areHex = all (`elem` (['0' .. '9'] ++ ['a' .. 'f'])) <$> rgb

eyeColor :: Map String String -> Bool
eyeColor m = orFalse $ correct <$> M.lookup "ecl" m
  where
    correct x = x `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

passport :: Map String String -> Bool
passport m = orFalse $ correct <$> M.lookup "pid" m
  where
    correct xs = ((==9) . length) xs && all (`elem` ['0' .. '9']) xs

orFalse :: Maybe Bool -> Bool
orFalse (Just True) = True
orFalse _ = False
