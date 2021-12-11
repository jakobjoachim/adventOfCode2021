module Days.Day9Task2 where

import Data.Set (Set, elemAt, empty, fromList, union, member, delete, deleteAt, difference, filter)
import Prelude hiding (filter)
import Data.List (sort)

type Matrix = [[Int]]
type Coordinate = (Int, Int)

runTask :: Matrix -> Int
runTask m = product $ take 3 $ reverse $ sort (findBasins m empty 0 (coordinateSet m) [])

findBasins :: Matrix -> Set Coordinate -> Int -> Set Coordinate -> [Int] -> [Int]
findBasins _ coordsToDo basinCount remaining result | coordsToDo == empty && remaining == empty  = basinCount : result
findBasins m coordsToDo basinCount remaining result | coordsToDo == empty                        = findBasins m (fromList [elemAt 0 remaining]) 0 (deleteAt 0 remaining) (basinCount : result)
findBasins m coordsToDo _          remaining result | m `at` elemAt 0 coordsToDo == 9            = findBasins m empty 0 remaining result
findBasins m coordsToDo basinCount remaining result                                              =
  let currentCoord  = elemAt 0 coordsToDo
      neighbours    = findNeighbours remaining currentCoord
      newRemaining  = remaining `difference` neighbours
      newTodo       = delete currentCoord $ coordsToDo `union` filter (\x -> m `at` x /= 9) neighbours
  in findBasins m newTodo (basinCount + 1) (delete currentCoord newRemaining) result

findNeighbours :: Set Coordinate -> Coordinate -> Set Coordinate
findNeighbours remaining (x,y) = filter (`member` remaining) $ fromList [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

at :: Matrix -> (Int, Int) -> Int
at m (x, y) = (m !! y) !! x

coordinateSet :: Matrix -> Set Coordinate
coordinateSet m = fromList [(x, y) | x <- [0 .. length (head m) -1], y <- [0 .. length m -1]]
