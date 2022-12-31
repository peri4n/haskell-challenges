{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module AdventOfCode.Year2022.Day7 (solveA, solveB) where

import Data.List as L
import AdventOfCode.Year2022.Day7.Parser (Session, session, Command (Cd, Ls), FsToken (FileToken, DirToken))
import qualified Control.Applicative as PS
import Data.Maybe
import Data.Text
import Data.Text.IO as TIO
import qualified Text.Megaparsec as P
import AdventOfCode.Year2022.Day7.Types (Name, Size)

-- solution

data FSItem = File Name Size | Folder Name [FSItem] deriving (Show)

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb])

fsCd :: Name -> FSZipper -> FSZipper
fsCd subDirName (Folder name items, cs) =
  let (ls, item : rs) = L.break isDirName items
   in (item, FSCrumb name ls rs : cs)
  where
    isDirName (Folder s _) = s == subDirName
    isDirName _ = False

fsInsert :: [FSItem] -> FSZipper -> FSZipper
fsInsert items (Folder name _, cs) = (Folder name items, cs)

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

emptyTree :: FSZipper
emptyTree = (Folder "/" [], [])

topMost :: FSZipper -> FSZipper
topMost r@(item, []) = r
topMost z = topMost $ fsUp z

root :: FSZipper -> FSItem
root fs =
    let (item, []) = topMost fs
    in
        item

applyInteraction :: Command -> FSZipper -> FSZipper
applyInteraction (Cd "/")  = topMost
applyInteraction (Cd "..") = fsUp
applyInteraction (Cd name) = fsCd name
applyInteraction (Ls ls)   = fsInsert $ L.map convertDirContent ls

convertDirContent :: FsToken -> FSItem
convertDirContent (DirToken name)     = Folder name []
convertDirContent (FileToken size name) = File name size

build :: Text -> FSItem
build s = case P.runParser session "" s of
    Right steps -> root $ L.foldl (flip applyInteraction) emptyTree steps
    Left  err   -> error $ show err

dataSize :: FSItem -> Size
dataSize (File _ size) = size
dataSize (Folder name ls) = sum $ L.map dataSize ls

allFolders :: (FSItem -> a) -> FSItem ->  [a]
allFolders f (File _ _)          = []
allFolders f fd@(Folder name ls) = f fd:L.concatMap (allFolders f) ls

part1 :: Text -> Size
part1 s = sum $ L.filter (<= 100000) $ allFolders dataSize $ build s

part2 :: Text -> Size
part2 s =
    let fs = build s
        sizes = allFolders dataSize fs
        usedSize = L.head sizes -- root is head
        freeSize = 70000000 - usedSize
        missing = 30000000 - freeSize
    in L.minimum (L.filter (>= missing) sizes)

solveA :: IO Int
solveA = part1 <$> TIO.readFile "data/2022/day7.txt"

solveB :: IO Int
solveB = part2 <$> TIO.readFile "data/2022/day7.txt"
