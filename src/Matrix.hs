module Matrix where
  
import Data.Maybe
import Data.List (find)

-- x Coord, y Coord, currentValue, counter)
type Element = (Int, Int, Int, Int)
type Matrix = [Element]

at :: Matrix -> (Int, Int) -> Maybe Element
at m coordinate = find (\a -> coord a == coordinate) m

value :: Element -> Int
value (_,_,x,_) = x
  
coord :: Element -> (Int, Int)
coord (x,y,_,_) = (x,y)

counter :: Element -> Int
counter (_,_,_,x) = x
  
elementWithValueExists :: Matrix -> Int -> Bool
elementWithValueExists m v = isJust (find (\a -> value a == v) m)

changeValueAt :: Matrix -> (Int,Int) -> (Int -> Int) -> Matrix
changeValueAt m z fun = [if z == (x,y) then (x,y,fun v,c) else e | e@(x,y,v,c) <- m]

fromList :: [[Int]] -> Matrix
fromList xs = [(x, y, (xs !! y) !! x, 0) | x <- [0 .. length (head xs) -1], y <- [0 .. length xs -1]]