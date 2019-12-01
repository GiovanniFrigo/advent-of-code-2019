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