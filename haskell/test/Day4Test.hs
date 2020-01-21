module Day4Test
  ( day4tests
  )
where

import           Test.HUnit
import           Day4

day4tests = TestList [part1Test]

part1Test :: Test
part1Test = TestLabel "Day 4 - Part 1 " $ TestList $ map testExample examples1
  where testExample (i, o) = TestCase (assertEqual (show i) (day4a i) o)

examples1 :: [(String, Int)]
examples1 = [("abcdef", 609043), ("pqrstuv", 1048970)]

