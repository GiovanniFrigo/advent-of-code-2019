-- https://adventofcode.com/2019/day/3
-- Crossed Wires

import Common (splitOn, toInt, (?), contiguousPairs)
import Data.Maybe (catMaybes)

type Point = (Int, Int)
type Segment = (Point, Point)

toMovement :: String -> Point
toMovement ('R':xs) = ( (toInt xs), 0)
toMovement ('L':xs) = (-(toInt xs), 0)
toMovement ('U':xs) = (0,  (toInt xs))
toMovement ('D':xs) = (0, -(toInt xs))

move :: Point -> Point -> Point
move (ax,ay) (bx,by) = (ax+bx, ay+by)

applyMoves :: Point -> [Point] -> [Point]
applyMoves currPos (m:xs) = let nextPos = move currPos m in nextPos:(applyMoves nextPos xs)
applyMoves _ [] = [] 

isVertical :: Point -> Point -> Bool
isVertical (ax,ay) (bx,by) = ax == bx

isHorizontal :: Point -> Point -> Bool
isHorizontal (ax,ay) (bx,by) = ay == by

between :: Int -> Int -> Int -> Bool
between a b x = (a < b) ? (a <= x && x <= b) $ between b a x 

intersects :: Point -> Point -> Point -> Point -> Bool
intersects (ax,ay) (a'x,a'y) (bx,by) (b'x,b'y) = 
    -- two vertical or two horizontal segments will never intersect
    isVertical (ax,ay) (a'x,a'y) /= isVertical (bx,by) (b'x,b'y) && 
    (
        (
            isVertical (ax,ay) (a'x,a'y) && between ay a'y by && between bx b'x ax
        ) || (
            isHorizontal (ax,ay) (a'x,a'y) && between ax a'x bx && between by b'y ay
        )
    )

intersect :: Point -> Point -> Point -> Point -> Point
intersect (ax,ay) (a'x,a'y) (bx,by) (b'x,b'y) = 
    isVertical (ax,ay) (a'x,a'y) ? (ax, by) $ (bx, ay)

getIntersection :: (Segment, Segment) -> Maybe Point
getIntersection ((p1, p2), (q1, q2)) = intersects p1 p2 q1 q2 ? Just (intersect p1 p2 q1 q2) $ Nothing

getIntersections :: [(Segment, Segment)] -> [Point]
getIntersections combos = catMaybes $ map getIntersection combos

combineElements :: [a] -> [b] -> [(a,b)]
combineElements listA listB = [(x, y) | x <- listA, y <- listB]

manhattan :: Point -> Int
manhattan (x, y) = abs x + abs y

part1 = do
    contents <- readFile "input.txt"
    let (path1:path2:_) = map (splitOn ',') $ lines contents
    let segments1 = applyMoves (0, 0) $ map toMovement path1
    let segments2 = applyMoves (0, 0) $ map toMovement path2
    let segmentPairs1 = contiguousPairs segments1
    let segmentPairs2 = contiguousPairs segments2
    let combos = combineElements segmentPairs1 segmentPairs2
    return $ minimum $ map manhattan $ getIntersections combos

segmentLength :: Segment -> Int
segmentLength ((ax, ay), (bx, by)) = isVertical (ax, ay) (bx, by) ? abs (ay - by) $ abs (ax - bx)

pointInLine :: Segment -> Point -> Bool
pointInLine ((ax,ay), (bx,by)) (px,py) = isVertical (ax,ay) (bx,by) ? 
    (px == ax && between ay by py) $ (py == ay && between ax bx px)

getTiming :: [Segment] -> Point -> Int
getTiming ((a, b):xs) p = if pointInLine (a,b) p 
    then segmentLength (a, p)
    else segmentLength (a, b) + getTiming xs p
getTiming [] _ = error "unsolvable"

part2 = do
    contents <- readFile "input.txt"
    let (path1:path2:_) = map (splitOn ',') $ lines contents
    let segments1 = applyMoves (0, 0) $ map toMovement path1
    let segments2 = applyMoves (0, 0) $ map toMovement path2
    let segmentPairs1 = contiguousPairs segments1
    let segmentPairs2 = contiguousPairs segments2
    let combos = combineElements segmentPairs1 segmentPairs2
    let intersectionPoints = getIntersections combos
    -- we need these to correctly calculate the wires starting from 0,0
    let segmentPairs1FromOrigin = ((0,0),(head segments1)):segmentPairs1
    let segmentPairs2FromOrigin = ((0,0),(head segments2)):segmentPairs2
    let pointsTiming = map (\p -> getTiming segmentPairs1FromOrigin p + getTiming segmentPairs2FromOrigin p) intersectionPoints
    return $ minimum pointsTiming


main = do
    putStrLn "Solving.."
    solution1 <- part1
    putStrLn $ "Part 1: " ++ show solution1
    solution2 <- part2
    putStrLn $ "Part 2: " ++ show solution2 
