module Grid2D where

import Prelude hiding (Left, Right)
import Data.Array (Array, (//))
import qualified Data.Array as Array
import Utils (zipWithIndexStarting)
import Data.List (groupBy, transpose)

type Pos = (Int, Int)
type Grid a = Array Pos a

xCoord (x, _) = x
yCoord (_, y) = y

dim :: (Pos -> Int) -> Grid a -> Int
dim f g =
  case Array.bounds g of
    (low, high) -> ((f high) - (f low)) + 1

height = dim xCoord
width  = dim yCoord

within :: Grid a -> Pos -> Bool
within g = Array.inRange (Array.bounds g)

manhattanDistance :: Pos -> Pos -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- List -> Grid

parse :: [String] -> Grid Char
parse = fromRows

fromAxis :: (Int -> Int -> Pos) -> [[a]] -> Grid a
fromAxis idx xs =
  let
    numberedList =
      do
        (i, ys) <- zipWithIndexStarting 0 xs
        (j, c) <- zipWithIndexStarting 0 ys
        return (idx i j, c)

    lvl1Len = length xs
    lvl2Len = length $ head xs
  in
    Array.array ((0, 0), (idx (lvl1Len-1) (lvl2Len-1))) numberedList

fromRows :: [[a]] -> Grid a
fromRows = fromAxis (,)

fromColumns :: [[a]] -> Grid a
fromColumns = fromAxis (flip (,))

-- Grid -> List

intoAssocRows :: Grid a -> [[(Pos, a)]]
intoAssocRows = groupBy (\((x1, _), _) ((x2, _), _) -> x1 == x2) . Array.assocs

intoAssocColumns :: Grid a -> [[(Pos, a)]]
intoAssocColumns = transpose . intoAssocRows

intoRows :: Grid a -> [[a]]
intoRows = map (map snd) . intoAssocRows

intoColumns :: Grid a -> [[a]]
intoColumns = transpose . intoRows

-- Direction

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

neighbours :: Pos -> [Pos]
neighbours pos = map (flip towards $ pos) allDirections

neighboursWithin :: Grid a -> Pos -> [Pos]
neighboursWithin g = filter (within g) . neighbours

-- Show

showCharGrid :: Grid Char -> String
showCharGrid g =
  do
    ((x, y), c) <- Array.assocs g
    if y == 0 && x /= 0
      then '\n':c:[]
      else return c
