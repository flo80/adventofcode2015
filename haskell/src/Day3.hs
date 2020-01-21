module Day3
  ( day3a
  , day3b
  , day3run
  )
where

import           Linear.V2
import           Data.Set                                 ( Set )
import qualified Data.Set                      as Set
import           Data.Foldable                            ( length )

day3run :: IO ()
day3run = do
  contents <- readFile "inputs/day3"
  putStr "Day 3 - Part 1: "
  print $ day3a contents
  putStr "Day 3 - Part 2: "
  print $ day3b contents
  putStrLn ""

type Move = V2 Int
parseInput :: String -> [Move]
parseInput = map parseChar
 where
  parseChar '^' = V2 0 1
  parseChar 'v' = V2 0 (-1)
  parseChar '<' = V2 (-1) 0
  parseChar '>' = V2 1 0

start :: Move
start = V2 0 0

day3a :: String -> Int
day3a = length . Set.fromList . positions . parseInput
  where positions = scanl (+) start

day3b :: String -> Int
day3b =
  Data.Foldable.length
    . Set.fromList
    . concatMap positions
    . createLists
    . unzip
    . split
    . parseInput
 where
  createLists (a, b) = [a, b]
  split :: [Move] -> [(Move, Move)]
  split (a : b : xs) = (a, b) : split xs
  split (a     : []) = [(a, start)]
  split []           = []
  positions = scanl (+) start

