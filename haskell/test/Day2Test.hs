module Day2Test
  ( day2tests
  )
where

import           Test.HUnit
import           Day2

day2tests = TestList [part1Test, part2Test]

part1Test :: Test
part1Test = TestLabel "Day 2 - Part 1 " $ TestList $ map testExample examples1
  where testExample (i, o) = TestCase (assertEqual (show i) (day2a i) o)

examples1 :: [(String, Int)]
examples1 = [("2x3x4", 58), ("1x1x10", 43)]

part2Test :: Test
part2Test = TestLabel "Day 2 - Part 2 " $ TestList $ map testExample examples2
  where testExample (i, o) = TestCase (assertEqual (show i) (day2b i) o)

examples2 :: [(String, Int)]
examples2 = [("2x3x4", 34), ("1x1x10", 14)]
