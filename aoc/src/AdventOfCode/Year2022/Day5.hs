module AdventOfCode.Year2022.Day5 where

import qualified Control.Applicative as PS
import Data.IntMap.Strict as M
import Data.List as L
import Data.Maybe
import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, many, sepBy, try)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Byte as P
import qualified Text.Megaparsec.Char as PC
import Text.Regex.Applicative ((<|>))

-- Input parsing
type Parser = Parsec Void String

type Stack = Int

type Label = Char

type Slot = Maybe Label

type State = IntMap [Label]

crate' :: Parser Slot
crate' = P.between open close name
  where
    open = PC.char '['
    name = Just <$> PC.upperChar
    close = PC.char ']'

noCrate :: Parser Slot
noCrate = Nothing <$ P.count 3 (PC.char ' ')

crate :: Parser Slot
crate = try $ noCrate <|> crate'

crateLine :: Parser [Maybe Char]
crateLine = crate `sepBy` PC.char ' ' <* PC.eol

crateLines :: Parser [[Maybe Char]]
crateLines = PS.some crateLine

number :: Parser Int
number = read <$> PS.some PC.numberChar

stack :: Parser Stack
stack = try $ P.between space space number
  where
    space = PC.char ' '

stackLine :: Parser [Stack]
stackLine = stack `sepBy` PC.char ' ' <* PC.eol

state :: Parser State
state = do
  cs <- crateLines
  ss <- stackLine
  let state = M.fromList $ ss `zip` L.map catMaybes (L.transpose cs)
  return state

type From = Int

type Count = Int

type To = Int

type Move = (From, Count, To)

move :: Parser Move
move = do
  m <- PC.string "move " *> number <* PC.char ' '
  f <- PC.string "from " *> number <* PC.char ' '
  t <- PC.string "to " *> number <* PC.eol
  return (m, f, t)

type Program = (State, [Move])

program :: Parser Program
program = do
  st <- state
  _ <- PC.eol
  moves <- P.many move
  return (st, moves)

-- Solution
type Step = State -> Move -> State

step1 :: Step
step1 s (count, from, to) = M.adjust (\s -> reverse stax ++ s) to collect
  where
    stax = take count (fromJust (M.lookup from s))
    collect = M.adjust (drop count) from s

step2 :: Step
step2 s (count, from, to) = M.adjust (stax ++) to collect
  where
    stax = take count (fromJust (M.lookup from s))
    collect = M.adjust (drop count) from s

solve :: Step -> Program -> String
solve step (s, m) = topOfStacks $ L.foldl' step s m
  where
    topOfStacks m = L.map snd $ M.toList $ M.map L.head m

prog :: IO Program
prog = do
  content <- readFile "data/2022/day5.txt"
  return $ fromJust $ P.parseMaybe program content

solveA :: IO String
solveA = solve step1 <$> prog

solveB :: IO String
solveB = solve step2 <$> prog
