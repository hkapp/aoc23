import Control.Exception (assert)
import Data.List (groupBy, find, foldl', sortOn)
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Control.Monad (join)

main =
  do
    test
    realData <- readFile "../data/day5.data.txt"
    let (seeds, mappings) = parse $ lines realData
    print $ part1 seeds mappings
    print $ part2 seeds mappings

-- test

x <!> y = assert (x == y) (return ())

test =
  do
    testData <- readFile "../data/day5.test.txt"
    let (seeds, mappings) = parse $ lines testData

    let oneMapping = head mappings
    let oneRange = head oneMapping
    False <!> inRange oneRange 97
    True  <!> inRange oneRange 98
    True  <!> inRange oneRange 99
    False <!> inRange oneRange 100

    Nothing <!> applyRange oneRange 97
    Just 50 <!> applyRange oneRange 98
    Just 51 <!> applyRange oneRange 99
    Nothing <!> applyRange oneRange 100

    applyMapping oneMapping 0  <!> 0
    applyMapping oneMapping 1  <!> 1
    applyMapping oneMapping 48 <!> 48
    applyMapping oneMapping 49 <!> 49
    applyMapping oneMapping 50 <!> 52
    applyMapping oneMapping 51 <!> 53
    applyMapping oneMapping 96 <!> 98
    applyMapping oneMapping 97 <!> 99
    applyMapping oneMapping 98 <!> 50
    applyMapping oneMapping 99 <!> 51

    applyMapping oneMapping 79 <!> 81
    applyMapping oneMapping 14 <!> 14
    applyMapping oneMapping 55 <!> 57
    applyMapping oneMapping 13 <!> 13

    location mappings 79 <!> 82
    location mappings 14 <!> 43
    location mappings 55 <!> 86
    location mappings 13 <!> 35

    part1 seeds mappings <!> 35

    part2 seeds mappings <!> 46

    putStrLn "Tests completed"

-- parseSeeds

type Seeds = [Int]

parseSeeds :: String -> Seeds
parseSeeds = map read . splitByChar ' ' . removePrefix "seeds: "

removePrefix :: (Eq a) => [a] -> [a] -> [a]
removePrefix prefix text =
  let
    n = length prefix
  in
    assert (take n text == prefix) (drop n text)

splitByChar :: Char -> String -> [String]
splitByChar = splitBy

-- parseRange

data RangeMapping = RangeMapping {
  destStart   :: Int,
  sourceStart :: Int,
  rangeLength :: Int
} deriving Show;

parseRange :: String -> RangeMapping
parseRange s =
  case map read $ splitByChar ' ' s of
    d:s:l:[] -> RangeMapping d s l

-- parseMapping

type Mapping = [RangeMapping]

parseMapping :: [String] -> Mapping
parseMapping = map parseRange

-- parse

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy c = map (filter ((/=) c)) . groupBy (\a b -> b /= c)

parse :: [String] -> (Seeds, [Mapping])
parse lines =
  let
    components = splitBy [] lines

    seedsSection = (\xs -> assert (length xs == 1) (head xs)) $ head components
    seeds = parseSeeds seedsSection

    -- Note: remove the "a-to-b map" line
    mappingsSection = map tail (tail components)
    mappings = map parseMapping mappingsSection
  in
    (seeds, mappings)

-- applyMapping

applyMapping :: Mapping -> Int -> Int
applyMapping m n =
  let
    anyRangeApplied = join $ find isJust $ map (flip applyRange n) m
  in
    -- Apply the default `id` mapping
    fromMaybe n anyRangeApplied

inRange :: RangeMapping -> Int -> Bool
inRange range n = n >= (sourceStart range) && n < (sourceStart range + rangeLength range)

applyRange :: RangeMapping -> Int -> Maybe Int
applyRange range n | inRange range n = Just $ destStart range + (n - sourceStart range)
applyRange _ _ = Nothing

-- location

location :: [Mapping] -> Int -> Int
location ms n = foldl' (\x m -> applyMapping m x) n ms

-- part1

part1 :: Seeds -> [Mapping] -> Int
part1 seeds mappings = minimum $ map (location mappings) seeds

-- buildSeedRanges

type Seed = Int

data SeedRange = SeedRange {
  seedLow   :: Seed,
  seedCount :: Int
} deriving Show

buildSeedRanges :: [Seed] -> [SeedRange]
buildSeedRanges (x:y:zs) = (SeedRange x y) : (buildSeedRanges zs)
buildSeedRanges [] = []

-- assignSubRanges

assignSubRanges :: [SeedRange] -> [RangeMapping] -> [(SeedRange, Maybe RangeMapping)]

-- No seed ranges left
assignSubRanges [] _ = []

-- No mappings left
assignSubRanges q [] = map (\s -> (s, Nothing)) q

-- The seed range is empty
assignSubRanges (s:q) ms | seedCount s == 0 = assignSubRanges q ms

-- The mapping range is empty
assignSubRanges q (m:z) | rangeLength m == 0 = assignSubRanges q z

-- The seed range starts before the mapping range
assignSubRanges (s:q) (m:z) | seedLow s < sourceStart m =
  let
    newLen = min (seedCount s) (sourceStart m - seedLow s)
    s' = SeedRange (seedLow s) newLen
    remS = SeedRange (seedLow s + newLen) (seedCount s - newLen)
  in
    (s', Nothing) : (assignSubRanges (remS:q) (m:z))

-- The seed range starts after the mapping range (strict)
assignSubRanges (s:q) (m:z) | seedLow s > sourceStart m =
  let
    offset = min (rangeLength m) (seedLow s - sourceStart m)
    m' = RangeMapping (destStart m + offset) (sourceStart m + offset) (rangeLength m - offset)
  in
    assignSubRanges (s:q) (m':z)

-- Both ranges start at the same value
assignSubRanges (s:q) (m:z) | seedLow s == sourceStart m =
  let
    newLen = min (seedCount s) (rangeLength m)
    s' = SeedRange (seedLow s) newLen
    remS = SeedRange (seedLow s + newLen) (seedCount s - newLen)

    m' = RangeMapping (destStart m) (sourceStart m) newLen
    remM = RangeMapping (destStart m + newLen) (sourceStart m + newLen) (rangeLength m - newLen)
  in
    (s', Just m') : (assignSubRanges (remS:q) (remM:z))

assignSubRanges' :: [SeedRange] -> [RangeMapping] -> [(SeedRange, Maybe RangeMapping)]
assignSubRanges' qs ms = assignSubRanges (sortOn seedLow qs) (sortOn sourceStart ms)

-- applyRangeMapping

applyRangeMapping :: [RangeMapping] -> [SeedRange] -> [SeedRange]
applyRangeMapping ms qs =
  let
    rangePairs = assignSubRanges' qs ms
    f (s, m) = flip SeedRange (seedCount s) $ applyMapping (maybeToList m) (seedLow s)
  in
    map f rangePairs

-- rangeLocations

rangeLocations :: [SeedRange] -> [Mapping] -> [SeedRange]
rangeLocations = foldl' (flip applyRangeMapping)

-- part2

part2 seeds ms = minimum $ map seedLow $ rangeLocations (buildSeedRanges seeds) ms
