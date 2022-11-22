module Main where

import System.Environment
import Problems.Problem1 as P1
import Problems.Problem2 as P2
import Problems.Problem3 as P3
import Problems.Problem4 as P4
import Problems.Problem5 as P5
import Problems.Problem6 as P6
import Problems.Problem7 as P7
import Problems.Problem8 as P8
import Problems.Problem9 as P9
import Problems.Problem10 as P10
import Problems.Problem11 as P11
import Problems.Problem12 as P12
import Problems.Problem13 as P13
import Problems.Problem14 as P14
import Problems.Problem15 as P15
import Problems.Problem16 as P16
import Problems.Problem17 as P17
import Problems.Problem18 as P18
import Problems.Problem19 as P19
import Problems.Problem22 as P22
import Problems.Problem23 as P23
import Problems.Problem27 as P27
import Problems.Problem31 as P31
import Problems.Problem32 as P32
import Problems.Problem35 as P35
import Problems.Problem36 as P36
import Problems.Problem37 as P37
import Problems.Problem41 as P41
import Problems.Problem42 as P42
import Problems.Problem43 as P43
import Problems.Problem50 as P50
import Problems.Problem87 as P87

main = do
  p <- getArgs
  solution <- case head p of
              "1" -> P1.solve
              "2" -> P2.solve
              "3" -> P3.solve
              "4" -> P4.solve
              "5" -> P5.solve
              "6" -> P6.solve
              "7" -> P7.solve
              "8" -> P8.solve
              "9" -> P9.solve
              "10" -> P10.solve
              "11" -> P11.solve
              "12" -> P12.solve
              "13" -> P13.solve
              "14" -> P14.solve
              "15" -> P15.solve
              "16" -> P16.solve
              "17" -> P17.solve
              "18" -> P18.solve
              "19" -> P19.solve
              "22" -> P22.solve
              "23" -> P23.solve
              "27" -> P27.solve
              "31" -> P31.solve
              "32" -> P32.solve
              "35" -> P35.solve
              "36" -> P36.solve
              "37" -> P37.solve
              "41" -> P41.solve
              "42" -> P42.solve
              "43" -> P43.solve
              "50" -> P50.solve
              "87" -> P87.solve
              _ -> return 0
  if solution == 0 then putStrLn $ "I have no solution Euler problem " ++ head p else print solution
