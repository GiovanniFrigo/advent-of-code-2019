-- https://adventofcode.com/2019/day/4
-- Secure Container

import Common (splitOn, digits, toInt)

monotonicDigits :: [Int] -> Bool
monotonicDigits (a:b:xs) = a <= b && monotonicDigits (b:xs)
monotonicDigits _ = True 

doubleDigits :: [Int] -> Bool
doubleDigits (a:b:xs) = if a == b then True else doubleDigits (b:xs)
doubleDigits _ = False

countValidPasswords :: (Int -> Bool) -> Int -> Int -> Int
countValidPasswords passwordValidator currentPassword maxPassword = 
    if currentPassword == maxPassword then 0 else
        if passwordValidator currentPassword 
        then 1 + countValidPasswords passwordValidator (currentPassword + 1) maxPassword
        else countValidPasswords passwordValidator (currentPassword + 1) maxPassword

part1 = do
    contents <- readFile "input.txt"
    let (min:max:_) = map toInt $ splitOn '-' contents 
    let validPassword p = let d = digits p in monotonicDigits d && doubleDigits d
    return $ countValidPasswords validPassword min max

-- alternate version of doubleDigits discarting repetitions
doubleDigitsNoRep :: [Int] -> Bool
doubleDigitsNoRep list@(a:b:c:xs) = 
    if a == b then 
        if b == c then doubleDigitsNoRep (dropWhile (==a) list)
        else True 
    else doubleDigitsNoRep (b:c:xs)
doubleDigitsNoRep (a:b:[]) = a == b
doubleDigitsNoRep (a:[]) = False
doubleDigitsNoRep _ = False

part2 = do
    contents <- readFile "input.txt"
    let (min:max:_) = map toInt $ splitOn '-' contents 
    let validPassword p = let d = digits p in monotonicDigits d && doubleDigitsNoRep d
    return $ countValidPasswords validPassword min max

main = do
    putStrLn "Solving.."
    solution1 <- part1
    putStrLn $ "Part 1: " ++ show solution1
    solution2 <- part2
    putStrLn $ "Part 2: " ++ show solution2 