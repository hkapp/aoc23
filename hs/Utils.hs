module Utils where

import Control.Exception (assert)
import Data.List (groupBy)

x <!> y | x == y = return ()
x <!> y = error $ (show x) ++ " != " ++ (show y)

splitByChar :: Char -> String -> [String]
splitByChar = splitBy

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy c = filter (not . null) . map (filter ((/=) c)) . groupBy (\a b -> b /= c)

zipWithIndexStarting :: Int -> [a] -> [(Int, a)]
zipWithIndexStarting start = zip [start..]

overlappingPairs :: [a] -> [(a, a)]
overlappingPairs (x:y:zs) = (x, y):(overlappingPairs $ y:zs)
overlappingPairs _ = []

takeUntilIncl :: (a -> Bool) -> [a] -> [a]
takeUntilIncl p (x:xs) | p x = [x]
takeUntilIncl p (x:xs) = x:(takeUntilIncl p xs)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil f = takeWhile (not . f)

pairsWithoutReplacement :: [a] -> [(a, a)]
pairsWithoutReplacement xs =
  let
    numbered = zipWithIndexStarting 0 xs
  in
    map (\((_, l), (_, r)) -> (l, r)) . filter (\((i, _), (j, _)) -> i < j) $ cartesianProduct numbered numbered

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys =
  do
    x <- xs
    y <- ys
    return (x, y)
