module Common where

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

-- a modified version of `words`, to split at commas
commaSeparatedWords   :: String -> [String]
commaSeparatedWords s =  case dropWhile (==',') s of
                          "" -> []
                          s' -> w : commaSeparatedWords s''
                                where (w, s'') = break (==',') s'

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