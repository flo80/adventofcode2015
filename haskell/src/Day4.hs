module Day4
  ( day4a
  , day4b
  , day4run
  )
where

import qualified Crypto.Hash.MD5               as MD5
                                                          ( hash )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.UTF8          as BS
import qualified Data.ByteString.Base16        as BS16


day4run :: IO ()
day4run = do
  contents <- readFile "inputs/day4"
  putStr "Day 4 - Part 1: "
  print $ day4a contents
  putStr "Day 4 - Part 2: "
  print $ day4b contents
  putStrLn ""



hash :: String -> Int -> String
hash secret = BS.toString . BS16.encode . MD5.hash . s . BS.fromString . show
  where s = BS.append $ BS.fromString secret

isValid :: Int -> String -> Bool
isValid nr = all ((==) '0') . take nr

day4a :: String -> Int
day4a secret = head $ take 1 $ filter (isValid 5 . hash secret) [1 ..]

day4b :: String -> Int
day4b secret = head $ take 1 $ filter (isValid 6 . hash secret) [1 ..]

