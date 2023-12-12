import Utils (splitByChar, (<!>))
import Data.List (group)
import Control.Monad (join)

main =
  do
    test

    realData <- readFile "../data/day12.data.txt"
    let inputPairs = parse $ lines realData

    print $ part1 inputPairs

    print $ part2 $ take 1 inputPairs

-- test

test =
  do
    testData <- readFile "../data/day12.test.txt"
    let inputPairs = parse $ lines testData

    [1, 4, 1, 1, 4, 10] <!> map (uncurry countArrangements) inputPairs
    21 <!> part1 inputPairs

    (expandSprings ".#") <!> ".#?.#?.#?.#?.#"
    (expandReport [1]) <!> [1,1,1,1,1]

    525152 <!> part2 inputPairs

    putStrLn "Tests completed"

-- parse

type Spring = Char

parse :: [String] -> [([Spring], Report)]
parse = map parseLine

parseLine :: String -> ([Spring], Report)
parseLine lines =
    case splitByChar ' ' lines of
        springs:nums:[] -> (springs, map read $ splitByChar ',' nums)

-- possibleSprings

possibleSprings :: [Spring] -> [[Spring]]

possibleSprings ('?':xs) =
    let
        ys = possibleSprings xs
        prefix c = map ((:) c) ys
    in
        (prefix '.') ++ (prefix '#')

possibleSprings (x:xs) = map ((:) x) (possibleSprings xs)

possibleSprings [] = [[]]

-- report

type Report = [Int]

report :: [Spring] -> Report
report = map length . filter (\xs -> (head xs) == '#') . group

-- countArrangements

countArrangements :: [Spring] -> Report -> Int
countArrangements springs expectedReport = length $ filter ((==) expectedReport) $ map report $ driveReplacement springs expectedReport
-- countArrangements springs expectedReport = length $ filter ((==) expectedReport) $ map report $ possibleSprings springs

-- part1

part1 = sum . map (uncurry countArrangements)

-- validPrefix

-- validPrefix :: [Spring] -> Int -> Bool
-- validPrefix springs len = all maybeDamaged $ take len springs

-- maybeDamaged :: Spring -> Bool
-- maybeDamaged '.' = False
-- maybeDamaged '?' = True
-- maybeDamaged '#' = True

-- -- buildValidArrangements

-- -- TODO check the following char
-- -- TODO enforce next char to be '.' if needed

-- buildValidArrangements :: [Spring] -> Report -> [[Spring]]
-- buildValidArrangements springs (n:ns) =
--     let
--         rec springs (n:ns) =
--         matches =
--             case tryMatchReport springs n of
--                 Just (prefix, suffix) -> map ((:) prefix) $ buildValidArrangements suffix ns
--                 Nothing -> []
--         regular = buildValidArrangements (tail springs) (n:ns)
--     in
--         filter (\ys -> expectedReport == report ys) (matches ++ regular)

-- tryMatchReport :: [Spring] -> Int -> Maybe ([Spring], [Spring])

-- -- When the first spring is working for sure, we can never build a group
-- tryMatchReport ('.':otherSprings) groupSize = Nothing

-- tryMatchReport springs groupSize =
--     let
--         (considered, rem) = splitAt groupSize springs
--         -- TODO the second part may be empty
--         (shouldBeBroken, shouldBeWorking:[]) = splitAt (groupSize - 1) considered
--         -- FIXME this needs to cover all the permutations for the count to be correct
--         satisfiedPrefix = reverse $ '#':(take groupSize $ repeat '.')
--     in
--         -- if alwaysBroken shouldBeWorking
--         --     -- This input is unsatisfyable, ever:
--         --     --   The (n+1)th spring is always working
--         --     --
--         --     then
--         if all maybeBroken shouldBeBroken && maybeWorking shouldBeWorking
--             then Just (satisfiedPrefix, rem)
--             else Nothing

-- Another attempt

driveReplacement :: [Spring] -> Report -> [[Spring]]

driveReplacement springs _ | all ((/=) '?') springs = [springs]

driveReplacement springs expectedReport =
    let
        (alreadyReplaced, remainingWork) = span ((/=) '?') springs
        replaceFirst ('?':xs) = map (\c -> c:xs) ['.', '#']
    in
        do
            newSuffix <- replaceFirst remainingWork
            let newSprings = alreadyReplaced ++ newSuffix
            if partialMatch newSprings expectedReport
                then driveReplacement newSprings expectedReport
                else []

partialMatch :: [Spring] -> Report -> Bool
partialMatch springs expectedReport =
    let
        replacedSoFar = takeWhile ((/=) '?') springs
        reportSoFar = report replacedSoFar
        realityVsExpectation = zip reportSoFar expectedReport

        cmpUpToLast [] = True
        cmpUpToLast ((real, expected):[]) = real <= expected
        cmpUpToLast ((real, expected):xs) = (real == expected) && (cmpUpToLast xs)
    in
        cmpUpToLast realityVsExpectation

-- expand

expandSprings :: [Spring] -> [Spring]
expandSprings = tail . join . map ((:) '?') . take 5 . repeat

expandReport :: Report -> Report
expandReport = join . take 5 . repeat

-- part2

part2 = part1 . map (\(s, r) -> (expandSprings s, expandReport r))