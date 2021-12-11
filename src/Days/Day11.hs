module Days.Day11 where

import Lib (rmdups)
import Matrix

runTaskOne :: [[Int]] -> Int
runTaskOne = sum . map counter . applyNTimes 100 (propagateNines . incrementAll) . fromList

runTaskTwo :: [[Int]] -> Int
runTaskTwo = runUntilSync 0 . fromList

runUntilSync :: Int -> Matrix -> Int
runUntilSync c m =
  if 1 == length (rmdups $ map value m)
    then c
    else runUntilSync (c + 1) (propagateNines $ incrementAll m)

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes n f x = iterate f x !! n

incrementAll :: Matrix -> Matrix
incrementAll m = [(x, y, v + 1, z) | (x, y, v, z) <- m]

propagateNines :: Matrix -> Matrix
propagateNines matrix =
  if not $ matrix `elementWithValueExists` 10
    then matrix
    else propagateNines $ applySurroundingCoords (applyNines matrix) (surroundingCoords matrix)

surroundingCoords :: Matrix -> [(Int, Int)]
surroundingCoords m =
  concat
    [ [(x -1, y -1), (x, y -1), (x + 1, y -1), (x -1, y), (x + 1, y), (x -1, y + 1), (x, y + 1), (x + 1, y + 1)]
      | (x, y) <- map coord $ filter (\x -> value x == 10) m
    ]

applyNines :: Matrix -> Matrix
applyNines m = [if v == 10 then (x, y, 0, c + 1) else e | e@(x, y, v, c) <- m]

applySurroundingCoords :: Matrix -> [(Int, Int)] -> Matrix
applySurroundingCoords = foldl (\m c -> changeValueAt m c spreadEnergy)

spreadEnergy :: Int -> Int
spreadEnergy 0 = 0
spreadEnergy 10 = 10
spreadEnergy x = x + 1
