module Numbers.Combinatorics
  ( permute
  ) where

import Data.List (delete)

permute :: Eq a => [a] -> [[a]]
permute [] = [[]]
permute xs = concatMap (\x -> map (x:) $ permute $ delete x xs) xs

