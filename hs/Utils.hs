module Utils where

import Control.Exception (assert)
import Data.List (groupBy)

x <!> y = assert (x == y) (return ())

splitByChar :: Char -> String -> [String]
splitByChar = splitBy

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy c = filter (not . null) . map (filter ((/=) c)) . groupBy (\a b -> b /= c)
