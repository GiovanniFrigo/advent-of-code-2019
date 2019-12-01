-- http://adventofcode.com/2019/day/1
-- The Tyranny of the Rocket Equation

import Common (linesAsInt)

calculateFuel :: Int -> Int
calculateFuel mass = mass `quot` 3 - 2 

part1 = do
    contents <- readFile "input.txt"
    return $ foldr (+) 0 $ map (calculateFuel) $ linesAsInt contents

calculateFuel' :: Int -> Int
calculateFuel' mass
    | mass < 7 = 0
    | otherwise = let f = calculateFuel mass in f + (calculateFuel' f)

part2 = do
    contents <- readFile "input.txt"
    return $ foldr (+) 0 $ map (calculateFuel') $ linesAsInt contents

main = do
    putStrLn "Solving.."
    solution1 <- part1
    putStrLn $ "Part 1: " ++ show solution1
    solution2 <- part2
    putStrLn $ "Part 2: " ++ show solution2