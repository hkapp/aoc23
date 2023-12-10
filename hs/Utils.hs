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
