import Data.Array (Array)
import Grid2D (Pos)
import qualified Grid2D
import Utils (splitBy, zipWithIndexStarting, (<!>))
import Data.List (find)
import Data.Maybe (fromMaybe)

main =
  do
    test

    realData <- readFile "../data/day13.data.txt"
    let grids = parse $ lines realData

    print $ part1 grids

    -- print $ part2 $ take 2 inputPairs

-- test

test =
  do
    testData <- readFile "../data/day13.test.txt"
    let grids = parse $ lines testData
    -- let (grid1:grid2:[]) = grids
    -- putStrLn $ Grid2D.showCharGrid grid1
    -- putStrLn ""
    -- putStrLn $ Grid2D.showCharGrid grid2
    -- print $ map (fmap (length . fst) . findRowMirror) grids
    -- print $ map (fmap (length . fst) . findColMirror) grids

    405 <!> part1 grids

    putStrLn "Tests completed"

-- parse

type Grid = Array Pos Tile
type Tile = Char

parse :: [String] -> [Grid]
parse lines = map Grid2D.parse $ splitBy [] lines

-- findMirror

findMirror :: [[Tile]] -> Maybe ([[Tile]], [[Tile]])
findMirror =
    let
        mirror ([], r) = False
        mirror (l, []) = False
        mirror (l, r)  = all (uncurry (==)) $ zip (reverse l) r
        -- mirror (l, r)  = all (uncurry (/=)) $ reflection (l, r)
    in
        find mirror . splits

findRowMirror :: Grid -> Maybe ([[Tile]], [[Tile]])
findRowMirror = findMirror . Grid2D.intoRows

findColMirror :: Grid -> Maybe ([[Tile]], [[Tile]])
findColMirror = findMirror . Grid2D.intoColumns

-- findMirror :: Int -> [[Tile]] -> Maybe ([[Tile]], [[Tile]])
-- findMirror ndiff =
    -- let
        -- diffCount ([], r) = Nothing
        -- diffCount (l, []) = Nothing
        -- -- TODO need to flatten the reflection before counting the differences
        -- diffCount (l, r)  = Just $ length $ filter (uncurry (/=)) $ join $ map  $ reflection (l, r)
        -- -- diffCount (l, r)  = Just $ length $ filter (uncurry (/=)) $ reflection (l, r)
    -- in
        -- find (\p -> (diffCount p) == (Just ndiff)) . splits
        -- -- fmap fst . find (mirror . snd) . zipWithIndexStarting 1 . splits

-- findRowMirror :: Int -> Grid -> Maybe ([[Tile]], [[Tile]])
-- findRowMirror n = findMirror n . Grid2D.intoRows

-- findColMirror :: Int -> Grid -> Maybe ([[Tile]], [[Tile]])
-- findColMirror n = findMirror n . Grid2D.intoColumns

splits :: [a] -> [([a], [a])]
splits xs = map ((flip splitAt) xs) [0..(length xs)]

-- part1

countBeforeMirror :: Maybe ([[Tile]], [[Tile]]) -> Int
countBeforeMirror = fromMaybe 0 . fmap (length . fst)

part1 gs =
    let
        f n mir = sum $ map ((*) n) $ map (countBeforeMirror . mir) gs
    in
        (f 1 findColMirror) + (f 100 findRowMirror)

-- part2

-- part2 = part1 . map fixSmudge

-- fixSmudge

-- reflection :: ([[Tile]], [[Tile]]) -> [([Tile], [Tile])]
-- reflection (l, r) = zip (reverse l) r

-- fixSmudge :: Grid -> Grid
-- fixSmudge g =
    -- let
        -- smudged = oneJust (findRowMirror 1 g) (findColMirror 1 g)

