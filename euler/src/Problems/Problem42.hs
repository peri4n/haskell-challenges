module Problems.Problem42 where

import Data.Char (ord)
import Numbers.Series (triangles)

solve :: IO Int
solve = do
  content <- readFile "data/p042_words.txt"
  let ws = read $ "[" ++ content ++ "]" :: [String]
      tws = [x | x <- ws, wordValue x `elem` take 400 triangles]
  return $ length tws


wordValue :: String -> Int
wordValue word = sum $ toInts word
    where toInts = map (\x -> ord x - ord 'A' + 1)

