module Days.Day5 where

import Input.Day5Input
import Data.Set (fromList)
  
type Coordinate = (Int, Int)

runTask :: Int
runTask = 
   let x = allCoords input
   in length x  - length (fromList x)

allCoords :: [(Coordinate, Coordinate)] -> [Coordinate]
allCoords xs = concat [allCoords' x [] | x <- xs]

allCoords' :: (Coordinate, Coordinate) -> [Coordinate] -> [Coordinate]
allCoords' (from, to) xs = if from == to then from : xs else allCoords' (nextCoord from to, to) (from : xs)

nextCoord :: Coordinate -> Coordinate -> Coordinate
nextCoord (a, b) (x, y) = (getCloser a x, getCloser b y)

getCloser :: Int -> Int -> Int
getCloser from to = from - signum (from - to)
