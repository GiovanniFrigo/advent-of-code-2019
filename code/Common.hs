module Common where

import Data.Char

-- an infix ternary operator: `cond ? truthy $ falsey`
(?) :: Bool -> a -> a -> a
True  ? x = const x
False ? _ = id

-- convert string to int
toInt :: String -> Int
toInt x = read x :: Int

readSignedInt ::  String -> Int
readSignedInt ('+':value) = toInt value
readSignedInt value = toInt value

linesAsInt :: String -> [Int]
linesAsInt contents = map toInt $ lines contents


-- splits an integer into its digits
digits :: Int -> [Int]
digits = map digitToInt . show


-- a modified version of `words`, to split at a given char
splitOn :: Char -> String ->  [String]
splitOn c s =  case dropWhile (==c) s of
	              "" -> []
	              s' -> w : splitOn c s''
	                    where (w, s'') = break (==c) s'

-- reads a string and splits it at each comma, then parses to int
stringToIntList :: String -> [Int]
stringToIntList list = map toInt $ commaSeparatedWords list

-- combine the elements of a list
combineElements :: [a] -> [(a, a)]
combineElements list = [(x, y) | x <- list, y <- list]

-- returns the given list zipped with the element's indexes
indexedList :: [a] -> [(Int, a)]
indexedList list = zip [0..] list

-- solution template
part1 = do
    contents <- readFile "input.txt"
    return $ lines contents