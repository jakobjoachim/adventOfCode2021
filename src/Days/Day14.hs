module Days.Day14 where

import Data.Foldable (find)
import Data.List (group, groupBy, sort)
import Data.Maybe (fromJust)
import Lib (splitOn)

type Polymer = (Char, Char)

type PolymerTable = [(Polymer, Char)]

runTask :: String -> Int
runTask xs =
  let ys = sort $ countElements $ applyNTimes 40 reactRow (map (\x -> (x, 1)) $ groupPolymers "CKKOHNSBPCPCHVNKHFFK") (createPolymerTable xs) 
   in last ys - head ys

createPolymerTable :: String -> PolymerTable
createPolymerTable xs = [((head line, line !! 1), line !! 6) | line <- splitOn (== '\n') xs]

reactRow :: PolymerTable ->  [(Polymer, Int)] -> [(Polymer, Int)]
reactRow ys = aggregateBy sum . foldl (\res x -> reactPolymer x ys ++ res) []

reactPolymer :: (Polymer, Int) -> PolymerTable -> [(Polymer, Int)]
reactPolymer (x@(a, b), i) ys =
  let newElement = snd $ fromJust $ find (\(y, _) -> y == x) ys
   in [((a, newElement), i), ((newElement, b), i)]

groupPolymers :: [a] -> [(a, a)]
groupPolymers xs = zip xs (tail xs)

applyNTimes :: Int -> (b -> a -> a) -> a -> b -> a
applyNTimes 0 _ a _ = a
applyNTimes n f a b = applyNTimes (n - 1) f (f b a) b

countElements :: [(Polymer, Int)] -> [Int]
countElements = map (\(_,x) -> ceiling (fromIntegral x / 2)) . aggregateBy sum . foldl (\y ((a, b), c) -> [(a, c), (b, c)] ++ y) []

aggregateBy :: Ord a => Ord b => ([b] -> b) -> [(a,b)] -> [(a,b)]
aggregateBy f xs =  [(fst $ head x, f $ map snd x) | x <- groupBy (\(a, _) (b, _) -> a == b) $ sort xs]