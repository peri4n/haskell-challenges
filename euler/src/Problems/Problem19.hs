module Problems.Problem19 where

type Date = (Int, Int, Int)

isSilvester :: Date -> Bool
isSilvester (31, 12, _) = True
isSilvester _ = False

nextYear :: Date -> Date
nextYear (_, _, y) = (1, 1, y+1)

nextMonth :: Date -> Date
nextMonth (_, m, y) = (1, m+1, y)

nextDay :: Date -> Date
nextDay (d, m, y) = (d+1, m, y)

has30Days :: Date -> Bool
has30Days (_, month, _) = month `elem` [4, 6, 9, 11]

has31Days :: Date -> Bool
has31Days (_, month, _) = month `elem` [1, 3, 5, 7, 8, 10, 12]

next :: Date -> Date
next d@(day, month, year)
  | isSilvester d = nextYear d
  | has30Days d = if day == 30 then nextMonth d else nextDay d
  | has31Days d = if day == 31 then nextMonth d else nextDay d
  | month == 2 && day == 28 && year `mod` 100 == 0 && year `mod` 400 /= 0 = nextMonth d
  | month == 2 && day == 28 && year `mod` 4 == 0 = nextDay d
  | month == 2 && day == 28 = nextMonth d
  | month == 2 && day == 29 = nextMonth d
  | month == 2 = nextDay d

solution :: Int
solution =
    let
      allDates = takeWhile (/= (1, 1, 2001)) $ iterate next (1, 1, 1900)
      dotw = cycle [1..7]
      both = zip dotw allDates
    in (length . filter (\(dw, (d, m, y)) -> 1901 <= y && y <= 2000 && d == 1 && dw == 7)) both

solve :: IO Int
solve = return solution
