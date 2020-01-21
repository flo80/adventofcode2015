module Day7
  ( day7a
  , day7b
  , day7run
  )
where


import           Data.Bits
import           Data.Map                                 ( Map
                                                          , (!)
                                                          )
import qualified Data.Map                      as Map
import           Data.Char                                ( isDigit )
import           Data.Word                                ( Word16 )
import           Debug.Trace
import           Data.Function.Memoize

day7run :: IO ()
day7run = do
  contents <- readFile "inputs/day7"
  putStr "Day 7 - Part 1: "
  print $ day7a contents
  putStr "Day 7 - Part 2: "
  print $ day7b contents
  putStrLn ""


type Value = Word16
data WireVal = Val Value | Ref String
data Circuit
  = Assign WireVal
  | And WireVal WireVal
  | Or WireVal WireVal
  | Not WireVal
  | LShift Int WireVal
  | RShift Int WireVal

type Wires = Map String Circuit


getValue :: Wires -> String -> Value
getValue wires target = mv target
 where
  mv = memoize getVal

  getVal :: String -> Value
  getVal name = getValue' $ wires ! name

  getValue' :: Circuit -> Value
  getValue' (Assign a  ) = gv a
  getValue' (And a b   ) = (gv a) .&. (gv b)
  getValue' (Or  a b   ) = (gv a) .|. (gv b)
  getValue' (Not a     ) = complement (gv a)
  getValue' (LShift x a) = shiftL (gv a) x
  getValue' (RShift x a) = shiftR (gv a) x

  gv :: WireVal -> Value
  gv (Val value) = value
  gv (Ref name ) = mv name



parseInput :: String -> Map String Circuit
parseInput contents = Map.fromList $ map (parseLine . words) $ lines contents
 where
  parseLine :: [String] -> (String, Circuit)
  parseLine [a, "->", name]              = (name, Assign (pv a))
  parseLine [a, "AND", b, "->", name]    = (name, And (pv a) (pv b))
  parseLine [a, "OR" , b, "->", name]    = (name, Or (pv a) (pv b))
  parseLine ["NOT", a, "->", name]       = (name, Not (pv a))
  parseLine [a, "LSHIFT", x, "->", name] = (name, LShift (read x) (pv a))
  parseLine [a, "RSHIFT", x, "->", name] = (name, RShift (read x) (pv a))

  pv :: String -> WireVal
  pv a | all isDigit a = Val (read a)
  pv a                 = Ref a


day7a :: String -> Value
day7a contents = getValue circuits "a" where circuits = parseInput contents

day7b :: String -> Value
day7b contents = getValue circuits "a"
 where
  temp     = parseInput contents
  valB     = Assign (Val (day7a contents))
  circuits = Map.insert "b" valB temp

