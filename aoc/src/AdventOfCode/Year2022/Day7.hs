module AdventOfCode.Year2022.Day7 where

import Data.Text

data Line
  = ChangeDirectory Text
  | ListDirectory
  | DirectoryEntry 
  | FileEntry
