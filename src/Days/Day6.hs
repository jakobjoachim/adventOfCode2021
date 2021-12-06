module Days.Day6 where

import Data.List.Utils
import Input.Day6Input

runTask :: Int
runTask = length (nextDay input 80)

nextDay :: [Int] -> Int -> [Int]
nextDay xs 0 = xs
nextDay xs y = nextDay ([ if x == 0 then 6 else x-1 | x <- xs] ++ replicate (countElem 0 xs) 8) (y-1)
