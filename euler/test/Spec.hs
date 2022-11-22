import Test.HUnit
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
import Problems.Problem18 as P18
import Problems.Problem19 as P19
import Problems.Problem22 as P22
import Problems.Problem23 as P23
import Problems.Problem27 as P27
import Problems.Problem32 as P32
import Problems.Problem35 as P35
import Problems.Problem36 as P36
import Problems.Problem37 as P37
import Problems.Problem41 as P41
import Problems.Problem42 as P42
import Problems.Problem43 as P43
import Problems.Problem50 as P50
import Problems.Problem87 as P87

problem1 = TestCase (assertEqual "Solution to problem 1" P1.solution 233168)

problem2 = TestCase (assertEqual "Solution to problem 2" P2.solution 4613732)

problem3 = TestCase (assertEqual "Solution to problem 3" P3.solution 6857)

problem4 = TestCase (assertEqual "Solution to problem 4" P4.solution 906609)

problem5 = TestCase (assertEqual "Solution to problem 5" P5.solution 232792560)

problem6 = TestCase (assertEqual "Solution to problem 6" P6.solution 25164150)

problem7 = TestCase (assertEqual "Solution to problem 7" P7.solution 104743)

problem8 = TestCase (assertEqual "Solution to problem 8" P8.solution 23514624000)

problem9 = TestCase (assertEqual "Solution to problem 9" P9.solution 31875000)

problem10 = TestCase (assertEqual "Solution to problem 10" P10.solution 142913828922)

problem11 = TestCase (assertEqual "Solution to problem 11" P11.solution 70600674)

problem12 = TestCase (assertEqual "Solution to problem 12" P12.solution 76576500)

problem13 = TestCase (assertEqual "Solution to problem 13" P13.solution 5537376230)

problem14 = TestCase (assertEqual "Solution to problem 14" P14.solution 837799)

problem15 = TestCase (assertEqual "Solution to problem 15" P15.solution 137846528820)

problem16 = TestCase (assertEqual "Solution to problem 16" P16.solution 1366)

problem18 = TestCase (assertEqual "Solution to problem 18" P18.solution 1074)

problem19 = TestCase (assertEqual "Solution to problem 19" P19.solution 171)

problem22 = TestCase (P22.solve >>= assertEqual "Solution to problem 22" 871198282)

problem23 = TestCase (assertEqual "Solution to problem 23" P23.solution 4179871)

problem27 = TestCase (assertEqual "Solution to problem 27" P27.solution (-59231))

problem32 = TestCase (assertEqual "Solution to problem 32" P32.solution 45228)

problem35 = TestCase (assertEqual "Solution to problem 35" P35.solution 55)

problem36 = TestCase (assertEqual "Solution to problem 36" P36.solution 872187)

problem37 = TestCase (assertEqual "Solution to problem 37" P37.solution 748317)

problem41 = TestCase (assertEqual "Solution to problem 41" P41.solution 7652413)

problem42 = TestCase (P42.solve >>= assertEqual "Solution to problem 42" 162)

problem43 = TestCase (assertEqual "Solution to problem 43" P43.solution 16695334890)

problem50 = TestCase (assertEqual "Solution to problem 50" P50.solution 997651)

problem87 = TestCase (assertEqual "Solution to problem 87" P87.solution 1097343)

main :: IO ()
main = runTestTTAndExit $ TestList [ problem1
                                   , problem2
                                   , problem3
                                   , problem4
                                   , problem5
                                   , problem6
                                   , problem7
                                   , problem8
                                   , problem9
                                   , problem10
                                   , problem11
                                   , problem12
                                   , problem13
                                   , problem14
                                   , problem15
                                   , problem16
                                   , problem18
                                   , problem19
                                   , problem22
                                   , problem23
                                   , problem27
                                   , problem32
                                   , problem35
                                   , problem36
                                   , problem41
                                   , problem42
                                   , problem43
                                   , problem50
                                   , problem87
                                   ]

