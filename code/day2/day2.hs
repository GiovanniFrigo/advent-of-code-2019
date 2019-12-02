-- https://adventofcode.com/2019/day/2
-- 1202 Program Alarm

import Common (stringToIntList)

setValue :: Int -> Int -> [Int]-> [Int]
setValue value pos memory = take (pos) memory ++ [value] ++ drop (pos+1) memory

compute :: [Int] -> Int -> [Int]
compute memory ip = let
            op = memory !! ip
            p0 = memory !! (ip + 1)
            p1 = memory !! (ip + 2)
            p2 = memory !! (ip + 3)
            nextip = ip + 4
    in case op of
        1 -> compute (setValue (memory !! p0 + memory !! p1) p2 memory) nextip
        2 -> compute (setValue (memory !! p0 * memory !! p1) p2 memory) nextip
        99 -> memory
        _ -> error "unsolvable"


setNounAndVerb :: (Int,Int) -> [Int] -> [Int]
setNounAndVerb (noun,verb) (i0:_:_:xs) = [i0,noun,verb] ++ xs

part1 = do
    contents <- readFile "input.txt"
    let program = stringToIntList contents
    let program1202 = setNounAndVerb (12,02) program
    return $ head $ compute program1202 0

expectedOutput = 19690720

bruteforceCompute :: (Int,Int) -> [Int] -> (Int,Int)
bruteforceCompute (100,100) program = error "unsolvable"
bruteforceCompute (noun,100) program = bruteforceCompute (noun+1,0) program
bruteforceCompute (noun,verb) program = let 
        p = setNounAndVerb (noun,verb) program
        result = head $ compute p 0
    in case result of
        19690720 -> (noun,verb)
        _ -> bruteforceCompute (noun,verb+1) program

part2 = do
    contents <- readFile "input.txt"
    let program = stringToIntList contents
    let (noun,verb) = bruteforceCompute (0,0) program
    return $ noun * 100 + verb

main = do
    putStrLn "Solving.."
    solution1 <- part1
    putStrLn $ "Part 1: " ++ show solution1
    solution2 <- part2
    putStrLn $ "Part 2: " ++ show solution2 