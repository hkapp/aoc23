import Prelude hiding (Left, Right)
import Data.Array (Array, (//))
import qualified Data.Array as Array
import Utils (zipWithIndexStarting, (<!>), overlappingPairs)
import Data.Functor ((<&>))
import Data.Maybe (fromJust, catMaybes)
import Data.List (find, sort)
import Control.Monad.State (State, put, get, evalState)
import Control.Exception (assert)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
import Data.Bifunctor (second)

main =
  do
    test

    realData <- readFile "../data/day10.data.txt"

    let (s, g) = replaceStart $ parse $ lines realData

    print $ part1 g s

    print $ part2 g s -- < 501

-- test

test =
  do
    testData1 <- readFile "../data/day10.test1.txt"
    let (s1, g1) = replaceStart $ parse $ lines testData1

    (1, 1) <!> s1
    'F' <!> tileAt g1 s1
    let testStartNeighbour f x = x <!> tileAt g1 (f s1)
    testStartNeighbour left '.'
    testStartNeighbour right '-'
    testStartNeighbour up '.'
    testStartNeighbour down '|'

    [(2, 1), (1, 2)] <!> destinations g1 s1
    [(1,1),(2,1),(3,1),(3,2),(3,3),(2,3),(1,3),(1,2),(1,1),(2,1)] <!> take 10 (walk g1 s1)

    4 <!> part1 g1 s1

    testData2 <- readFile "../data/day10.test2.txt"
    let (s2, g2) = replaceStart $ parse $ lines testData2

    8 <!> part1 g2 s2

    testData3 <- readFile "../data/day10.test3.txt"
    let (s3, g3) = replaceStart $ parse $ lines testData3

    let mainLoop3 = Set.fromList $ mainLoop g3 s3
    Set.fromList [(6, 2), (6, 3)] <!> areaAround g3 mainLoop3 (6, 2)
    Set.fromList [(6, 2), (6, 3)] <!> areaAround g3 mainLoop3 (6, 3)
    3 <!> length (areas g3 mainLoop3)
    -- putStrLn $ showSideAssignment g3 $ annotate $ mainLoop g3 s3
    4 <!> part2 g3 s3

    testData4 <- readFile "../data/day10.test4.txt"
    let (s4, g4) = replaceStart $ parse $ lines testData4
    let mainLoop4 = Set.fromList $ mainLoop g4 s4
    12 <!> length (areas g4 mainLoop4)
    sort [3, 1, 15, 1, 5, 1, 21, 1, 2, 2, 1, 7] <!> (sort $ map length $ areas g4 mainLoop4)
    8 <!> part2 g4 s4

    testData5 <- readFile "../data/day10.test5.txt"
    let (s5, g5) = replaceStart $ parse $ lines testData5
    let mainLoop5 = Set.fromList $ mainLoop g5 s5

    10 <!> part2 g5 s5

    putStrLn "Tests completed"

-- parse

{-
   y-->
  x
  |
  v
-}
type Grid = Array Pos Tile
type Pos = (Int, Int)
type Tile = Char

parse :: [String] -> Grid
parse lines =
  let
    numberedList =
      do
        (i, xs) <- zipWithIndexStarting 0 lines
        (j, c) <- zipWithIndexStarting 0 xs
        return ((i, j), c)
    width = length $ head lines
    height = length lines
  in
    Array.array ((0, 0), (height-1, width-1)) numberedList

-- Directions

data Direction = Left | Right | Up | Down
  deriving (Eq, Show)

left  (x, y) = (x,   y-1)
right (x, y) = (x,   y+1)
up    (x, y) = (x-1, y)
down  (x, y) = (x+1, y)

towards :: Direction -> Pos -> Pos
towards Left  = left
towards Right = right
towards Up    = up
towards Down  = down

allDirections = [Left, Right, Up, Down]

neighbours :: Grid -> Pos -> [(Direction, Pos)]
neighbours g pos =
  let
    allNeighbours = allDirections <&> (\d -> (d, towards d pos))
  in
    filter (within g . snd) allNeighbours

within :: Grid -> Pos -> Bool
within g = Array.inRange (Array.bounds g)

-- startPos

startPos :: Grid -> Pos
startPos g = fromJust $ fmap fst $ find (((==) 'S') . snd) $ Array.assocs g

-- replaceStart

replaceStart :: Grid -> (Pos, Grid)
replaceStart g =
  let
    s = startPos g
    s' = connectingPipe g s -- $ map (tileAt g . snd) $ neighbours g s
    g' = g // [(s, s')]
  in
    (s, g')

tileAt :: Grid -> Pos -> Tile
tileAt = (Array.!)

connectingPipe :: Grid -> Pos -> Tile
connectingPipe g pos =
  let
    neigh d = tileAt g (towards d pos)
  in
    if (neigh Right) == '-' && (neigh Down) == '|'
      then 'F'
      else
        if (neigh Right) == 'J' && (neigh Down) == '|'
          then 'F'
          else
            if (neigh Left) == 'F' && (neigh Down) == 'L'
              then '7'
              else
                if (neigh Right) == '7' && (neigh Down) == 'J'
                  then 'F'
                  else
                    if (neigh Left) == 'F' && (neigh Down) == '|'
                      then '7'
                      else undefined

-- step

step :: Grid -> State (Pos, Pos) ()
step g =
  do
    (prevPos, currPos) <- get
    -- Note: normally there can be only one value after the 'filter'
    -- For the initial step though, there will be two
    -- In this case, we just pick one such direction
    -- Also note: we don't want to go back
    let nextPos = head $ filter ((/=) prevPos) $ destinations g currPos
    put (currPos, nextPos)

destinations :: Grid -> Pos -> [Pos]
destinations g currPos =
  let
    currTile = tileAt g currPos
    exits = filter (within g . snd) $ map (\d -> (d, towards d currPos)) $ exitDirs currTile

    isValidExit (d, nextPos) =
      let
        nextTile = tileAt g nextPos
        validEnters = enterDirs nextTile
      in
        d `elem` validEnters

    validExits = filter isValidExit exits
  in
    map snd validExits

exitDirs :: Tile -> [Direction]
exitDirs '|' = [Up, Down]
exitDirs '-' = [Left, Right]
exitDirs 'L' = [Up, Right]
exitDirs 'J' = [Left, Up]
exitDirs '7' = [Left, Down]
exitDirs 'F' = [Down, Right]
exitDirs '.' = []

enterDirs :: Tile -> [Direction]
enterDirs = map opposite . exitDirs

opposite :: Direction -> Direction
opposite Up = Down
opposite Down = Up
opposite Left = Right
opposite Right = Left

-- walk

-- Note: the resuling list starts with 'pos'
walk :: Grid -> Pos -> [Pos]
walk g pos =
  let
    stepReturningPrevPos =
      do
        step g
        (prevPos, currPos) <- get
        return prevPos
  in
    (flip evalState) (pos, pos) $ sequence $ repeat stepReturningPrevPos

-- part1

mainLoop :: Grid -> Pos -> [Pos]
mainLoop g s =
  let
    loopTail = takeWhile ((/=) s) $ tail $ walk g s
  in
    s:loopTail

part1 g s =
  let
    loopMeat = mainLoop g s
    totLoopLength = length loopMeat
  in
    assert ((totLoopLength `mod` 2) == 0) (totLoopLength `div` 2)

-- areaAround

fixpoint :: (Eq a) => (a -> a) -> a -> a
fixpoint f z = fromJust $ fmap fst $ find (uncurry (==)) $ iterate (\(_, curr) -> (curr, f curr)) (z, f z)

areaAround :: Grid -> Set Pos -> Pos -> Set Pos
areaAround g mainLoop pos =
  let
    expand s =
      let
        xs = Set.toList s
        ys = xs >>= (filter (notIn mainLoop) . map snd . neighbours g)
      in
        Set.fromList (xs ++ ys)
  in
    fixpoint expand (Set.fromList [pos])

notIn :: (Ord a) => Set a -> a -> Bool
notIn = flip Set.notMember

debug :: (Show a) => a -> a
debug x = unsafePerformIO (print x >> return x)

-- areas

-- Note: the areas don't contain any entry for the main loop
areas :: Grid -> Set Pos -> [Set Pos]
areas g mainLoop =
  let
    startingSet = (Set.fromList $ Array.indices g) `Set.difference` mainLoop

    rec candidates | null candidates = []
    rec candidates =
      let
        anyCandidate = Set.elemAt 0 candidates
        a = areaAround g mainLoop anyCandidate
        remCandidates = candidates `Set.difference` a
      in
        a:(rec remCandidates)
  in
    rec startingSet

-- outside

type Area = Set Pos

outside :: Grid -> Area -> Bool
outside g a =
  let
    touchBorder x = length (neighbours g x) < 4
  in
    any touchBorder a

showGrid :: Grid -> String
showGrid g =
  do
    ((x, y), c) <- Array.assocs g
    if y == 0
      then '\n':c:[]
      else return c


-- annotate

data Side = LHS | RHS
  deriving (Eq, Show)

annotate :: [Pos] -> Map Pos Side
annotate mainLoop =
  let
    succs = (last mainLoop, head mainLoop):(overlappingPairs mainLoop)
    annotatedPositions =
      do
        (prev, next) <- succs
        attributeSidesToNeighbors prev next
  in
    Map.fromList annotatedPositions


attributeSidesToNeighbors :: Pos -> Pos -> [(Pos, Side)]
attributeSidesToNeighbors prevPos nextPos =
  let
    miniBatch (f1, s1) (f2, s2) = [(f1 prevPos, s1), (f1 nextPos, s1), (f2 prevPos, s2), (f2 nextPos, s2)]
  in
    case dirMoved prevPos nextPos of
      {- l l
         0 1
         r r
      -}
      Right -> miniBatch (up, LHS) (down, RHS)
      {- r r
         1 0
         l l
      -}
      Left -> miniBatch (up, RHS) (down, LHS)
      {- l 1 r
         l 0 r
      -}
      Up -> miniBatch (left, LHS) (right, RHS)
      {- r 0 l
         r 1 l
      -}
      Down -> miniBatch (right, LHS) (left, RHS)

{-
   y-->
  x
  |
  v
-}
dirMoved :: Pos -> Pos -> Direction
dirMoved (x1, y1) (x2, y2) =
  case (x2 - x1, y2 - y1) of
    ( 1, 0) -> Down
    (-1, 0) -> Up
    (0,  1) -> Right
    (0, -1) -> Left

showModGrid :: Grid -> [(Pos, Tile)] -> String
showModGrid g modPoints = showGrid (g // modPoints)

showSideAssignment :: Grid -> Map Pos Side -> String
showSideAssignment g sides =
  let
    sideAsChar LHS = 'l'
    sideAsChar RHS = 'r'
  in
    showModGrid g $ map (second sideAsChar) $ Map.toList sides

-- part2

part2 g s =
  let
    loop = mainLoop g s
    as = areas g $ Set.fromList loop
    sides = annotate loop
    areasWithSides = map (\a -> (a, areaSide sides a)) as
    outwardsSide = snd $ fromJust $ find (outside g . fst) areasWithSides
    innerAreas = map fst $ filter (\(_, s) -> s /= outwardsSide) areasWithSides
  in
    sum $ map length innerAreas

areaSide :: Map Pos Side -> Area -> Side
areaSide sides a = head $ catMaybes $ map (flip Map.lookup $ sides) $ Set.toList a
