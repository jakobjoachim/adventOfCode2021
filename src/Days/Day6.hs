module Days.Day6 where

import Data.List (sortBy)
import Input.Day6Input
import Lib

runTask :: Int
runTask = sum $ nextDay 256 $ 0 : [b | (_, b) <- sortBy (\(a, _) (b, _) -> compare a b) $ numberOfOccurrences input] ++ [0, 0, 0, 0]

nextDay :: Int -> [Int] -> [Int]
nextDay 0 xs = xs
nextDay y (x : xs) = nextDay (y - 1) $ take 6 xs ++ [(xs !! 6) + x] ++ [xs !! 7] ++ [x]
nextDay _ [] = []
