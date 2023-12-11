import qualified Grid2D
import Grid2D (Grid, width, height, Pos, neighboursWithin, up, left, xCoord, yCoord)
import Utils ((<!>), takeUntilIncl, takeUntil, pairsWithoutReplacement, zipWithIndexStarting)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Array as Array
import qualified Data.Bifunctor as Bifunctor
import Data.Bifunctor (Bifunctor)

main =
  do
    test

    realData <- readFile "../data/day11.data.txt"
    let space = parse $ lines realData

    print $ part1 space

    print $ part2 space

-- test

test =
  do
    testData <- readFile "../data/day11.test.txt"
    let space = parse $ lines testData
    let expanded1 = expand 2 space

    (width $ expandColumns 2 space) <!> ((width space) + (length $ filter emptySpace $ intoColumns space))
    (height $ expandRows 2 space) <!> ((height space) + (length $ filter emptySpace $ intoRows space))

    let p9 = (11, 5)
    let p5 = (6, 1)

    9 <!> shortestPathLength expanded1 (p9, p5)

    let p1 = (0, 4)
    let p7 = (10, 9)
    15 <!> shortestPathLength expanded1 (p1, p7)

    let p3 = (2, 0)
    let p6 = (7, 12)
    17 <!> shortestPathLength expanded1 (p3, p6)

    let p8 = (11, 0)
    5 <!> shortestPathLength expanded1 (p8, p9)

    374 <!> part1 space

    1030 <!> answer 10 space
    8410 <!> answer 100 space

    putStrLn "Tests completed"

-- parse

type Space = Grid Pixel
type Pixel = Char

parse :: [String] -> Space
parse = Grid2D.parse

-- expand

expand :: Int -> Space -> Space
expand amount = expandRows amount . expandColumns amount

expandAxis :: (Space -> [[Pixel]]) -> ([[Pixel]] -> Space) -> Int -> Space -> Space
expandAxis intoAxis fromAxis amount = fromAxis . listExpand emptySpace amount . intoAxis

expandRows :: Int -> Space -> Space
expandRows = expandAxis intoRows fromRows

expandColumns :: Int -> Space -> Space
expandColumns = expandAxis intoColumns fromColumns

intoAxis f  = map (map snd) . f
intoRows    = intoAxis Grid2D.intoRows
intoColumns = intoAxis Grid2D.intoColumns
fromRows    = Grid2D.fromRows
fromColumns = Grid2D.fromColumns

listExpand :: (a -> Bool) -> Int -> [a] -> [a]
listExpand p n xs =
  do
    x <- xs
    if p x
      then take n $ repeat x
      else return x

emptySpace :: Foldable t => t Pixel -> Bool
emptySpace = all ((==) '.')

-- shortestPathLength

shortestPathLength :: Space -> (Pos, Pos) -> Int
shortestPathLength space (start, dest) = Grid2D.manhattanDistance start dest

-- part1

galaxies :: Space -> [Pos]
galaxies = map fst . filter (\(_, px) -> px == '#') . Array.assocs

answer n space =
  let
    er = emptyRows space
    ec = emptyCols space
  in
    sum $ map (shortestPathLength2 space (er, ec) n) $ pairsWithoutReplacement $ galaxies space

part1 = answer 2

-- part2

part2 = answer 1000000

type EmptyAxis = Set Int
type EmptyRows = EmptyAxis
type EmptyCols = EmptyAxis

shortestPathLength2 :: Space -> (EmptyRows, EmptyCols) -> Int -> (Pos, Pos) -> Int
shortestPathLength2 space (emptyRows, emptyCols) expansion (start, dest) =
  let
    baseDistance = Grid2D.manhattanDistance start dest
    -- Note: multiply by (expansion - 1) because 1 crossing is already accounted for in the base distance
    slowdown slowZone crossedAreas = (expansion - 1) * (length $ filter ((flip Set.member) slowZone) crossedAreas)
    slowdownRow = slowdown emptyRows $ crossedRows (start, dest)
    slowdownCol = slowdown emptyCols $ crossedCols (start, dest)
  in
    baseDistance + slowdownRow + slowdownCol

valuesBetween :: (Int, Int) -> [Int]
valuesBetween (a, b) = [(min a b)..(max a b)]

crossedRows :: (Pos, Pos) -> [Int]
crossedRows = valuesBetween . mapBoth xCoord

crossedCols :: (Pos, Pos) -> [Int]
crossedCols = valuesBetween . mapBoth yCoord

mapBoth :: Bifunctor f => (a -> b) -> f a a -> f b b
mapBoth f = Bifunctor.first f . Bifunctor.second f

emptyAxis :: (Space -> [[Pixel]]) -> Space -> EmptyAxis
emptyAxis toAxis = Set.fromList . map fst . filter (emptySpace . snd) . zipWithIndexStarting 0 . toAxis

emptyRows = emptyAxis intoRows
emptyCols = emptyAxis intoColumns
