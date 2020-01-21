module Day2
  ( day2a
  , day2b
  , day2run
  )
where

import           Data.List.Split                          ( splitOn )

day2run :: IO ()
day2run = do
  contents <- readFile "inputs/day2"
  putStr "Day 2 - Part 1: "
  print $ day2a contents
  putStr "Day 2 - Part 2: "
  print $ day2b contents
  putStrLn ""

parseInput :: String -> [[Int]]
parseInput = map (map read . splitOn "x") . lines

day2a :: String -> Int
day2a = sum . map calc . parseInput
 where
  calc [l, w, h] =
    2 * l * w + 2 * w * h + 2 * h * l + minimum [l * w, w * h, h * l]

day2b :: String -> Int
day2b = sum . map calc . parseInput
  where calc [l, w, h] = 2 * minimum [l + w, l + h, w + h] + l * w * h
