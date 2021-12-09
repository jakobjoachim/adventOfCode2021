module Days.Day9 where

import Data.Maybe

type Matrix = [[Int]]

runTask :: Matrix -> Int
runTask m = let mins = findMins m (length (head m) -1 , length m -1) []
            in sum mins + length mins
--runTask m = m `at` (length (head m) -1 , length m -1)

-- i current coordinate
findMins :: Matrix -> (Int, Int) -> [Int] -> [Int]
findMins m xy@(0, 0) bs = addIfJust (isMin m xy) bs
findMins m xy@(x, 0) bs = findMins m (x - 1, length m - 1) (addIfJust (isMin m xy) bs)
findMins m xy@(x, y) bs = findMins m (x, y - 1) (addIfJust (isMin m xy) bs)

-- return number at coord if top,right,bottom,left are higher
isMin :: Matrix -> (Int, Int) -> Maybe Int
isMin m (x, y) =
  let num = m `at` (x, y)
   in if num `lessOrNothing` (m `at` (x -1, y)) && num `lessOrNothing` (m `at` (x + 1, y)) && num `lessOrNothing` (m `at` (x, y -1)) && num `lessOrNothing` (m `at` (x, y + 1)) then num else Nothing

lessOrNothing :: Maybe Int -> Maybe Int -> Bool
lessOrNothing x y | isNothing y = True
lessOrNothing x y               = x < y

at :: Matrix -> (Int, Int) -> Maybe Int
at _ (x, _) | x < 0 = Nothing
at _ (_, y) | y < 0 = Nothing
at m (x, _) | x >= length (head m) = Nothing
at m (_, y) | y >= length m = Nothing
at m (x, y) = Just $ (m !! y) !! x

addIfJust :: Maybe Int -> [Int] -> [Int]
addIfJust x ys | isNothing x = ys
addIfJust x ys = fromJust x : ys
