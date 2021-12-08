module Days.Day8 where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set, filter, intersection, isSubsetOf, singleton, size, take, unions)
import qualified Data.Set as Set
import Lib
import Prelude hiding (filter)

type Line = ([Set Char], [Set Char])

runTask :: String -> Int
runTask xs = task2 $ processInput xs

processInput :: String -> [Line]
processInput xs = [let y = splitOn (== '|') x in ([Set.fromList a | a <- words (head y)], [Set.fromList a | a <- words (y !! 1)]) | x <- splitOn (== '\n') xs]

task1 :: [Line] -> Int
task1 xs = size $ filter (\x -> size x == 2 || size x == 3 || size x == 4 || size x == 7) (unions [Set.fromList x | (_, x) <- xs])

task2 :: [Line] -> Int
task2 xs = sum [applyNumberMap (findNumbers a) b | (a, b) <- xs]

-- 1 = 2 letters;
-- 4 = 4 letters;
-- 7 = 3 letters;
-- 8 = 7 letters;
-- 6 =    6 letters; one letter of 1 missing => matching is letter bottom right (f)
-- 2 =        5 letters; contains (f)
-- 3 =        5 letters; contains all letters of 1
-- 5 =        5 letters; last 5 letter number;
-- 9 =            6 letters; contains exactly all letters of 1 + 5
-- 0 =                6 letters; last 6 letter number
findNumbers :: [Set Char] -> Map (Set Char) Char
findNumbers xs =
  let one = findFirst (\x -> size x == 2) xs
      four = findFirst (\x -> size x == 4) xs
      seven = findFirst (\x -> size x == 3) xs
      eight = findFirst (\x -> size x == 7) xs
      six = findFirst (\x -> size x == 6 && not (one `isSubsetOf` x)) xs
      two = findFirst (\x -> size x == 5 && not ((six `intersection` one) `isSubsetOf` x)) xs
      three = findFirst (\x -> size x == 5 && one `isSubsetOf` x && x /= two) xs
      five = findFirst (\x -> size x == 5 && x /= two && x /= three) xs
      nine = findFirst (\x -> size x == 6 && one `isSubsetOf` x && five `isSubsetOf` x) xs
      zero = findFirst (\x -> size x == 6 && x /= nine && x /= six) xs
   in Map.fromList [(zero, '0'), (one, '1'), (two, '2'), (three, '3'), (four, '4'), (five, '5'), (six, '6'), (seven, '7'), (eight, '8'), (nine, '9')]

applyNumberMap :: Map (Set Char) Char -> [Set Char] -> Int
applyNumberMap x ys = read [x ! y | y <- ys]
