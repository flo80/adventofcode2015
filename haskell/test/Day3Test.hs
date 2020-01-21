module Day3Test
  ( day3tests
  )
where

import           Test.HUnit
import           Day3

day3tests = TestList [part1Test, part2Test]

part1Test :: Test
part1Test = TestLabel "Day 3 - Part 1 " $ TestList $ map testExample examples1
  where testExample (i, o) = TestCase (assertEqual (show i) (day3a i) o)

examples1 :: [(String, Int)]
examples1 = [(">", 2), ("^>v<", 4), ("^v^v^v^v^v", 2)]

part2Test :: Test
part2Test = TestLabel "Day 3 - Part 2 " $ TestList $ map testExample examples2
  where testExample (i, o) = TestCase (assertEqual (show i) (day3b i) o)

examples2 :: [(String, Int)]
examples2 = [("^v", 3), ("^>v<", 3), ("^v^v^v^v^v", 11)]
