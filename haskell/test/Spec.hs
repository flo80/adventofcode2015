import           Test.HUnit
import           Day1Test
import           Day2Test
import           Day3Test
import           Day4Test

main :: IO Counts
main = runTestTT $ TestList [day4tests] --[day1tests, day2tests, day3tests,day4tests]
