import Prelude hiding (Left, Right)
import Data.Array (Array, (//))
import qualified Data.Array as Array
import Utils (zipWithIndexStarting, (<!>))
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Data.List (find)
import Control.Monad.State (State, put, get, evalState)
import Control.Exception (assert)

main =
  do
    test

    realData <- readFile "../data/day10.data.txt"

    let (s, g) = replaceStart $ parse $ lines realData

    print $ part1 g s

    -- print $ part2 oasiss

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

part1 g s =
  let
    loopMeat = takeWhile ((/=) s) $ tail $ walk g s
    totLoopLength = 1 + length loopMeat
  in
    assert ((totLoopLength `mod` 2) == 0) (totLoopLength `div` 2)
