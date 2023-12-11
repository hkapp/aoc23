import Data.Map (Map, (!))
import qualified Data.Map as Map
import Utils ((<!>), zipWithIndexStarting, takeUntil)
import Control.Monad (join)
import Data.List (transpose, find)
import qualified Data.Set as Set
import Control.Monad.State (state, evalState)
import Control.Exception (assert)
import Data.Maybe (fromJust)
import Data.Functor ((<&>))

main =
  do
    test

    realData <- readFile "../data/day8.data.txt"

    let (dirs, desert) = parse $ lines realData

    print $ part1 dirs desert

    print $ part2c dirs desert

-- test

test =
  do
    testData1 <- readFile "../data/day8.test1.txt"
    let (dirs1, desert1) = parse $ lines testData1

    "CCC" <!> move desert1 'R' startPos

    2 <!> part1 dirs1 desert1

    testData2 <- readFile "../data/day8.test2.txt"
    let (dirs2, desert2) = parse $ lines testData2

    6 <!> part1 dirs2 desert2

    testData3 <- readFile "../data/day8.test3.txt"
    let (dirs3, desert3) = parse $ lines testData3

    let multiplesOf n = map ((*) n) [1..]
    let limit = 100
    let limitedCheck xs ys = take limit xs <!> take limit ys
    let clock1 = ghostClock dirs3 desert3 "11A"
    let clock2 = ghostClock dirs3 desert3 "22A"
    limitedCheck (multiplesOf 2) clock1
    limitedCheck (multiplesOf 3) clock2

    let mergedClocks = clock1 `mergeSort` clock2
    limitedCheck (multiplesOf 6) mergedClocks

    6 <!> part2 dirs3 desert3
    6 <!> part2b dirs3 desert3
    6 <!> part2c dirs3 desert3

    putStrLn "Tests completed"

-- parse

type Direction = Char
type Pos = String
type Desert = Map Pos (Pos, Pos)

parse :: [String] -> ([Direction], Desert)
parse (dl:[]:mls) = (dl, parseMappings mls)

parseMappings :: [String] -> Desert
parseMappings =
  let
    parseMappingLine (a:b:c:' ':'=':' ':'(':d:e:f:',':' ':g:h:i:")") = ([a, b, c], ([d, e, f], [g, h, i]))
  in
    Map.fromList . map parseMappingLine

-- move

move :: Desert -> Direction -> Pos -> Pos
move desert dir currPos = pick dir (desert ! currPos)

pick :: Direction -> (a, a) -> a
pick 'L' = fst
pick 'R' = snd

startPos = "AAA"
destPos = "ZZZ"

-- wander

wander :: [Direction] -> Desert -> Pos -> [Pos]
wander (d:ds) desert currPos =
  let
    nextPos = move desert d currPos
  in
    currPos : (wander ds desert nextPos)

wander' :: [Direction] -> Desert -> [Pos]
wander' ds desert = wander (join $ repeat ds) desert startPos

-- part1

part1 ds desert = length $ takeWhile ((/=) destPos) $ wander' ds desert

-- recurrenceTime

recurrenceTime :: (Ord a) => [a] -> ([a], [a])
recurrenceTime xs =
  let
    repElem = fromJust $ firstRepeatedElem xs
    (prefix, repeatingSuffix) = break ((==) repElem) xs
    incompleteSuffix = takeWhile ((/=) repElem) $ tail repeatingSuffix
    suffix = assert (repElem == head repeatingSuffix) (repElem:incompleteSuffix)
  in
    (prefix, suffix)

mapAccum :: (Traversable t) => (b -> a -> (c, b)) -> b -> t a -> t c
mapAccum f z xs =
  let
    states = fmap (state . (flip f)) xs
    sequenced = sequence states
  in
    evalState sequenced z

findAccum :: (Traversable t) => (b -> a -> b) -> (b -> a -> Bool) -> b -> t a -> Maybe a
findAccum f p z = fmap fst . find snd . mapAccum (\accum x -> ((x, p accum x), f accum x)) z

firstRepeatedElem :: (Ord a) => [a] -> Maybe a
firstRepeatedElem = findAccum (flip Set.insert) (flip Set.member) Set.empty

-- ghostClock

-- Indexes start at 0
ghostClock :: [Direction] -> Desert -> Pos -> [Int]
ghostClock dirs desert ghostStart =
  let
    ghostPositions = wander (join $ repeat dirs) desert ghostStart
    dirCarets = map (\n -> n `mod` (length dirs)) [0..]
    ghostStates = ghostPositions `zip` dirCarets
    (beforeRecurrence, loop) = recurrenceTime ghostStates

    ticks = map fst . filter (isGhostDest . snd) . zipWithIndexStarting 0 . map fst
    earlyTicks = ticks beforeRecurrence
    loopTicks = ticks loop
    recurringTicks = [0..] >>= (\i -> loopTicks <&> (\t -> t + (length beforeRecurrence) + i * (length loop)))
  in
    earlyTicks ++ recurringTicks

-- part2c

part2c dirs desert =
  let
    clocks = map (ghostClock dirs desert) (ghostStarts desert)
    clockFreqs = map head clocks
  in
    foldl1 lcm clockFreqs


-- Unsuccessful attempts for part 2 --


-- mergeSort
-- Note: inputs must be sorted in ascending order

mergeSort :: (Ord a) => [a] -> [a] -> [a]
mergeSort (x:xs) (y:ys) | x < y  = mergeSort xs (y:ys)
mergeSort (x:xs) (y:ys) | x > y  = mergeSort (x:xs) ys
mergeSort (x:xs) (y:ys) | x == y = x:(mergeSort xs ys)
mergeSort [] _ = []
mergeSort _ [] = []

-- part2b

part2b dirs desert =
  let
    clocks = map (ghostClock dirs desert) (ghostStarts desert)
    simultaneousTicks = foldl1 mergeSort clocks
  in
    head simultaneousTicks




-- ghostWander

lastIs :: (Eq a) => a -> [a] -> Bool
lastIs x ys = x == (last ys)

ghostStarts :: Desert -> [Pos]
ghostStarts = filter (lastIs 'A') . Map.keys

ghostWander :: [Direction] -> Desert -> [[Pos]]
ghostWander dirs desert =
  let
    startPoints = ghostStarts desert
    repDirs = join $ repeat dirs
    ghostSeparate = map (wander repDirs desert) startPoints
    ghostSimultaneous = transpose ghostSeparate
  in
    ghostSimultaneous

-- isGhostDest

isGhostDest :: Pos -> Bool
isGhostDest = lastIs 'Z'

allAtDest :: [Pos] -> Bool
allAtDest = and . map isGhostDest

-- part2

part2 ds desert = length $ takeUntil allAtDest $ ghostWander ds desert
