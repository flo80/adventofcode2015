module Day1
    ( day1a
    , day1b
    , day1run
    )
where

day1run :: IO ()
day1run = do
    contents <- readFile "inputs/day1"
    putStr "Day 1 - Part 1: "
    print $ day1a contents
    putStr "Day 1 - Part 2: "
    print $ day1b contents
    putStrLn ""

parseInput :: String -> [Int]
parseInput = map parseChar
  where
    parseChar '(' = 1
    parseChar ')' = -1

day1a :: String -> Int
day1a = sum . parseInput

day1b :: String -> Int
day1b = (+) 1 . length . takeWhile (>= 0) . scanl1 (+) . parseInput
