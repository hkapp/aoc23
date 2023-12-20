import Data.Array (Array)
import Grid2D (Pos)
import qualified Grid2D
import Utils (splitBy, zipWithIndexStarting, (<!>), both, debug)
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

    405 <!> part1 grids

    400 <!> part2 grids

    putStrLn "Tests completed"

-- parse

type Grid = Array Pos Tile
type Tile = Char

parse :: [String] -> [Grid]
parse lines = map Grid2D.parse $ splitBy [] lines

-- findReflections

findReflections :: Grid -> [ReflectionAxis]
findReflections g = (map Row $ rowReflections g) ++ (map Col $ colReflections g)

axisReflections :: [[Tile]] -> [Reflection]
axisReflections =
    let
        mirror ([], r) = False
        mirror (l, []) = False
        mirror (l, r)  = all (uncurry (==)) $ zip (reverse l) r
        -- mirror (l, r)  = all (uncurry (/=)) $ reflection (l, r)
    in
        filter mirror . splits

splits :: [a] -> [([a], [a])]
splits xs = map ((flip splitAt) xs) [0..(length xs)]

rowReflections :: Grid -> [Reflection]
rowReflections = axisReflections . Grid2D.intoRows

colReflections :: Grid -> [Reflection]
colReflections = axisReflections . Grid2D.intoColumns

-- findMirror

findMirror :: Grid -> ReflectionAxis
findMirror = findExactlyOne (const True) . findReflections

-- part1

mirrorValue :: ReflectionAxis -> Int
mirrorValue m =
    let
        baseValue = length . fst
    in
        case m of
            Row r -> 100 * (baseValue r)
            Col c -> baseValue c

part1 = sum . map (mirrorValue . findMirror)

-- part2

part2 = sum . map (mirrorValue . fixSmudge)

-- fixSmudge

reflection :: ([[a]], [[a]]) -> [([a], [a])]
reflection (l, r) = zip (reverse l) r

fineGrainReflection :: ([[a]], [[a]]) -> [(a, a)]
fineGrainReflection = join . map (uncurry zip) . reflection

fixSmudge :: Grid -> ReflectionAxis
fixSmudge g = fromJust $ fromJust $ find isJust $ map (differentReflectionAxis (fromSingleton $ findReflections g)) $ map (invertAt g) $ candidateSmudges g

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
-- However, we still need to consider both as they might not both lead to a reflection axis
candidateSmudges :: Grid -> [Pos]
candidateSmudges = (=<<) pairToList . map head . filter (\difs -> (length difs) == 1) . allDiffs

pairToList :: (a, a) -> [a]
pairToList (l, r) = [l, r]

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

data ReflectionAxis = Row Reflection | Col Reflection
    deriving (Eq, Show)

type Reflection = ([[Tile]], [[Tile]])

differentReflectionAxis :: ReflectionAxis -> Grid -> Maybe ReflectionAxis
differentReflectionAxis prevReflectionAxis repairedMirror =
    let
        axisNeq a b = not $ axisEq a b

        innerEq a b = (length $ fst a) == (length $ fst b)
        axisEq (Row r1) (Row r2) = innerEq r1 r2
        axisEq (Col c1) (Col c2) = innerEq c1 c2
        axisEq _ _ = False
    in
        find (axisNeq prevReflectionAxis) $ findReflections repairedMirror
