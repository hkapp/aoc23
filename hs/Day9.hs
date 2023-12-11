import Utils (splitByChar, (<!>), overlappingPairs, takeUntilIncl)
import Data.List (foldl')

main =
  do
    test

    realData <- readFile "../data/day9.data.txt"

    let oasiss = parse $ lines realData

    print $ part1 oasiss

    print $ part2 oasiss

-- test

test =
  do
    let testData = ["0 3 6 9 12 15", "1 3 6 10 15 21", "10 13 16 21 30 45"]
    let allRows = parse testData
    let (row1:row2:row3:[]) = allRows

    substractionSequences row1 <!> [row1, [3, 3, 3, 3, 3], [0, 0, 0, 0]]
    18 <!> extrapolateForward (substractionSequences row1)

    substractionSequences row2 <!> [row2, [2, 3, 4, 5, 6], [1, 1, 1, 1], [0, 0, 0]]
    28 <!> extrapolateForward (substractionSequences row2)

    substractionSequences row3 <!> [row3, [3, 3, 5, 9, 15], [0, 2, 4, 6], [2, 2, 2], [0, 0]]
    68 <!> extrapolateForward (substractionSequences row3)

    114 <!> part1 allRows

    (-3) <!> extrapolateBackwards (substractionSequences row1)
    0 <!> extrapolateBackwards (substractionSequences row2)
    5 <!> extrapolateBackwards (substractionSequences row3)

    2 <!> part2 allRows

    putStrLn "Tests completed"

-- parse

parse :: [String] -> [[Int]]
parse = map (map read . splitByChar ' ')

-- successiveDifferences

successiveDifferences :: [Int] -> [Int]
successiveDifferences = map (\(x, y) -> y - x) . overlappingPairs

-- substractionSequences

substractionSequences :: [Int] -> [[Int]]
substractionSequences =
  let
    allZeroes = all ((==) 0)
  in
    takeUntilIncl allZeroes . iterate successiveDifferences

-- extrapolate

extrapolate :: ([Int] -> Int) -> (Int -> Int -> Int) -> [[Int]] -> Int
extrapolate pickOne combine = foldl' combine 0 . reverse . map pickOne

extrapolateForward :: [[Int]] -> Int
extrapolateForward = extrapolate last (+)

-- part1

solve f = sum . map (f . substractionSequences)

part1 = solve extrapolateForward

-- extrapolateBackwards

extrapolateBackwards :: [[Int]] -> Int
extrapolateBackwards = extrapolate head (\accum x -> x - accum)

-- part2

part2 = solve extrapolateBackwards
