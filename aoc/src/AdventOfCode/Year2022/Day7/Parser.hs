module AdventOfCode.Year2022.Day7.Parser (Command (Cd, Ls), Session, session, FsToken (FileToken, DirToken), parseCd) where

import Data.Text
import Data.Void (Void)
import Text.Megaparsec (Parsec, endBy, errorBundlePretty, many, sepBy, try)
import Text.Megaparsec.Char (char, digitChar, eol, numberChar, printChar, string)
import Text.ParserCombinators.ReadP (many1)
import Text.Regex.Applicative ((<|>))

type Parser = Parsec Void Text

data FsToken = DirToken Text | FileToken Int Text deriving (Eq, Show)

data Command = Cd Text | Ls [FsToken] deriving (Eq, Show)

type Session = [Command]

label :: Parser Text
label = pack <$> many printChar

number :: Parser Int
number = read <$> many numberChar

parseCd :: Parser Command
parseCd = Cd <$> try (string "cd " *> label <* eol)

parseLs :: Parser Command
parseLs = do
  string "ls" >> eol
  contents <- dirContent `endBy` eol
  return $ Ls contents

dirContent :: Parser FsToken
dirContent = dir <|> file
  where
    dir = DirToken <$> try (string "dir " *> label)
    file = do
      sz <- number
      char ' '
      FileToken sz <$> label

interaction :: Parser Command
interaction = string "$ " *> (parseCd <|> parseLs)

session :: Parser Session
session = many interaction
