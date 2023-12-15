import Utils (splitByChar, (<!>), debug)
import Data.List (group, transpose)
import Control.Monad (join)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State (State, get, put, evalState)
import Control.Exception (assert)

main =
  do
    test

    realData <- readFile "../data/day12.data.txt"
    let inputPairs = parse $ lines realData

    print $ part1 inputPairs

    let (springs1, report1) = head $ drop 1 inputPairs
    let instances = filter (\m -> report1 == report m) $ driveReplacement (springs1 ++ "?" ++ springs1) (report1 ++ report1)
    -- let instances = filter (\m -> report1 == report m) $ driveReplacement springs1 report1
    -- print $ instances
    -- print $ transpose instances
    -- let repeatedElement xs = foldr (\a b -> case b of { Just x | x == a -> Just x ; _ -> Nothing }) (Just $ head xs) xs
    -- print $ map repeatedElement $ transpose instances
    -- print $ filter ((==) expectedReport) $ map report $ driveReplacement springs expectedReport

    -- take 1: 1316327 (2s)
    -- take 2: 1195096364
    print $ part2 $ take 2 inputPairs

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
-- countArrangements springs expectedReport = length $ filter ((==) expectedReport) $ map report $ driveReplacement springs expectedReport
countArrangements = dpCount
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


-- Third attempt

type DP = Map DPS Int
type DPS = (Int, Int)

lookupOrCompute :: [Spring] -> [Int] -> State DP Int
lookupOrCompute springs remReport =
    do
        alreadyComputed <- get
        let key = (length springs, length remReport)
        case Map.lookup key alreadyComputed of
            Just x -> return x
            Nothing ->
                do
                    x <- statefulCompute springs remReport
                    put $ Map.insert key x alreadyComputed
                    return x

statefulCompute :: [Spring] -> [Int] -> State DP Int

statefulCompute springs (expectedGroupSize:remReport) =
    let
        initialSlicing = burgers expectedGroupSize springs
        constituentStates = map value $ filter mainValid $ takeWhile longEnough $ takeWhile validPrefix initialSlicing
        -- Note: we use 'take 1' rather than 'head' as 'remSprings' could be empty
        -- isValid (shouldBeWorking, shouldBeDamaged, remSprings) = (all canBeWorking shouldBeWorking) &&
                                                                -- (all canBeDamaged shouldBeDamaged) &&
                                                                -- (all canBeWorking $ take 1 remSprings)
        mainValid (shouldBeWorking, shouldBeDamaged, remSprings) = (all canBeDamaged shouldBeDamaged) &&
                                                                   (all canBeWorking $ take 1 remSprings)
        -- Note: we don't substract one because the suffix should contain a working delimitor
        longEnough (shouldBeWorking, shouldBeDamaged, remSprings) = (length remSprings) >= (sum $ map ((+) 1) remReport)
        validPrefix (shouldBeWorking, shouldBeDamaged, remSprings) = (all canBeWorking shouldBeWorking)
        -- Note: we don't need to care about the number of '?' in shouldBeDamaged, as they must all be set to '#' for this entire thing to work
        -- hence there is no longer a choice for '?'s here
        -- same reasoning goes for the first element of remSprings
        -- Note: we enforce that
        value (ignoredPrefix, shouldBeDamaged, remSprings) = lookupOrCompute (drop 1 remSprings) remReport
    in
        fmap sum $ sequence constituentStates

-- We don't have any groups to build anymore
-- Do we still have entries that would necessarily form a group?
-- If so, the entire decomposition is invalid
statefulCompute springs [] =
    if any alwaysDamaged springs
        then return 0
        else return 1

-- We don't have any springs, but still some groups to build
-- This can't be achieved, hence this decomposition is invalid
-- Note: this is redundant of the first case
-- statefulCompute [] (r:rs) = return 0

-- burgers
-- burgers are sandwiches with bread, meat, then bread
-- the meat is guaranteed to be of a given size, but there might not be bread on either side

burgers :: Int -> [a] -> [([a], [a], [a])]
burgers meatSize xs | (length xs) < meatSize = []
burgers meatSize xs =
    let
        validIndices = [0..((length xs) - meatSize)]

        splitTwice n =
            let
                (a, tmp) = splitAt n xs
                (b, c) = splitAt meatSize tmp
            in
                assert (length b == meatSize) (a, b, c)
    in
        map splitTwice validIndices

-- damaged info

canBeDamaged :: Spring -> Bool
canBeDamaged s = alwaysDamaged s || undecided s

canBeWorking :: Spring -> Bool
canBeWorking = not . alwaysDamaged

alwaysDamaged :: Spring -> Bool
alwaysDamaged = (==) '#'

undecided :: Spring -> Bool
undecided = (==) '?'

-- dpCount

dpCount :: [Spring] -> Report -> Int
dpCount springs expectedReport = (flip evalState) Map.empty $ statefulCompute springs expectedReport
