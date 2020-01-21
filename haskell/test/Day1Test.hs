module Day1Test
  ( day1tests
  )
where

import           Test.HUnit
import           Day1

day1tests = TestList [part1Test, part2Test]

part1Test :: Test
part1Test = TestLabel "Day 1 - Part 1 " $ TestList $ map testExample examples1
  where testExample (i, o) = TestCase (assertEqual (show i) (day1a i) o)

examples1 :: [(String, Int)]
examples1 =
  [ ("(())"   , 0)
  , ("()()"   , 0)
  , ("((("    , 3)
  , ("(()(()(", 3)
  , ("))(((((", 3)
  , ("())"    , -1)
  , ("))("    , -1)
  , (")))"    , -3)
  , (")())())", -3)
  ]

part2Test :: Test
part2Test = TestLabel "Day 1 - Part 2 " $ TestList $ map testExample examples2
  where testExample (i, o) = TestCase (assertEqual (show i) (day1b i) o)

examples2 :: [(String, Int)]
examples2 = [(")", 1), ("()())", 5)]
