import Data.Array (Array)
import Grid2D (Pos)
import qualified Grid2D
import Utils (splitBy, zipWithIndexStarting, (<!>), both)
import Data.List (find)
import Data.Maybe (fromMaybe, fromJust, isJust, maybeToList)
import Control.Monad (join)
import Data.Array ((//), (!))

main =
  do
    test

    realData <- readFile "../data/day13.data.txt"
    let grids = parse $ lines realData

    print $ part1 grids

    print $ part2 grids

-- test

test =
  do
    testData <- readFile "../data/day13.test.txt"
    let grids = parse $ lines testData
    let (grid1:grid2:[]) = grids
    -- putStrLn $ Grid2D.showCharGrid grid1
    -- putStrLn ""
    -- putStrLn $ Grid2D.showCharGrid grid2
    -- print $ map (fmap (length . fst) . findRowMirror) grids
    -- print $ map (fmap (length . fst) . findColMirror) grids

    405 <!> part1 grids

    let fixPos1 = (0, 0)
    -- True <!> elem fixPos1 (candidateSmudges grid1)

    let fixPos2 = (1, 4)
    -- True <!> elem fixPos2 (candidateSmudges grid2)

    let fixedGrid1 = invertAt grid1 fixPos1
    let fixedGrid2 = invertAt grid2 fixPos2
    -- putStrLn $ Grid2D.showCharGrid fixedGrid1
    -- putStrLn ""
    -- putStrLn $ Grid2D.showCharGrid fixedGrid2

    (Just 3) <!> fmap (length . fst) (findRowMirror fixedGrid1)
    (Just 5) <!> fmap (length . fst) (findColMirror fixedGrid1)
    (Just 1) <!> fmap (length . fst) (findRowMirror fixedGrid2)
    Nothing  <!> fmap (length . fst) (findColMirror fixedGrid2)

    400 <!> part2 grids

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

part2 =
    let
        value (Row r) = 100 * (baseValue r)
        value (Col c) = baseValue c
        baseValue = length . fst
    in
        sum . map (value . fixSmudge)

mirrorAxes :: Grid -> [ReflectionAxis]
mirrorAxes g =
    let
        extract wrap mir = map wrap $ maybeToList $ mir g
    in
        (extract Row findRowMirror) ++ (extract Col findColMirror)
    -- case (findRowMirror g, findColMirror g) of
        -- (Just r, Nothing)  -> Just $ Row r
        -- (Nothing, Just c)  -> Just $ Col c
        -- (Nothing, Nothing) -> Nothing
        -- _ -> Nothing -- is this good enough?

-- fixSmudge

reflection :: ([[a]], [[a]]) -> [([a], [a])]
reflection (l, r) = zip (reverse l) r

fineGrainReflection :: ([[a]], [[a]]) -> [(a, a)]
fineGrainReflection = join . map (uncurry zip) . reflection

fixSmudge :: Grid -> ReflectionAxis
fixSmudge g = fromJust $ findExactlyOne isJust $ map (differentReflectionAxis (fromSingleton $ mirrorAxes g)) $ map (invertAt g) $ candidateSmudges g

invertAt :: Grid -> Pos -> Grid
invertAt g pos = g // [(pos, invert (g ! pos))]

invert :: Tile -> Tile
invert '.' = '#'
invert '#' = '.'

findExactlyOne :: Show a => (a -> Bool) -> [a] -> a
findExactlyOne p = fromSingleton . filter p

fromSingleton :: Show a => [a] -> a
fromSingleton (x:[]) = x
fromSingleton xs = error $ "Not exactly one value in " ++ (show xs)

-- candidateSmudges

-- smudges are always symmetrical: they lead to the same reflection axis no matter which symmetrical point we choose to fix
candidateSmudges :: Grid -> [Pos]
-- candidateSmudges = (=<<) pairToList . map head . filter (\difs -> (length difs) == 1) . allDiffs
candidateSmudges = map fst . map head . filter (\difs -> (length difs) == 1) . allDiffs

-- pairToList :: (a, a) -> [a]
-- pairToList (l, r) = [l, r]

-- differences

-- differences always come in pair
-- we can either fix one or the other
reflectionDifferences :: [[(Pos, Tile)]] -> [[(Pos, Pos)]]
reflectionDifferences = map diffPairs . filter neitherEmpty . {- [([[(Pos, Tile)]], [[(Pos, Tile)]])] -} splits

rowDiffs = reflectionDifferences . Grid2D.intoAssocRows
colDiffs = reflectionDifferences . Grid2D.intoAssocColumns
allDiffs g = (rowDiffs g) ++ (colDiffs g)

neitherEmpty :: ([a], [a]) -> Bool
neitherEmpty (l, r) = (not $ null l) && (not $ null r)

diffPairs :: ([[(Pos, Tile)]], [[(Pos, Tile)]]) -> [(Pos, Pos)]
diffPairs = map (both fst) . filter (\((_, a), (_, b)) -> a /= b) . {- [((Pos, Tile), (Pos, Tile))] -} fineGrainReflection

-- oneTrueSmudge
-- That's the smudge that leads to a change in reflection axis

data ReflectionAxis = Row ([[Tile]], [[Tile]]) | Col ([[Tile]], [[Tile]])
    deriving (Eq, Show)

differentReflectionAxis :: ReflectionAxis -> Grid -> Maybe ReflectionAxis
differentReflectionAxis prevReflectionAxis repairedMirror =
    let
        axisNeq a b = not $ axisEq a b

        innerEq a b = (length $ fst a) == (length $ fst b)
        axisEq (Row r1) (Row r2) = innerEq r1 r2
        axisEq (Col c1) (Col c2) = innerEq c1 c2
        axisEq _ _ = False
    in
        find (axisNeq prevReflectionAxis) $ mirrorAxes repairedMirror
