module Lib (replace, rotate, numberOfOccurrences, splitOn, readFileAsNumberArray, findFirst, rmdups, readFileAsPairOfStrings) where

import Data.Char (digitToInt)
import Data.List (group, sort)

-- replace all matching elements in list with something else
replace :: Eq b => b -> b -> [b] -> [b]
replace a b = map (\x -> if a == x then b else x)

-- rotate a matrix 90 degrees
rotate :: Eq b => [[b]] -> [[b]]
rotate xs = rotate' xs []

rotate' :: Eq b => [[b]] -> [[b]] -> [[b]]
rotate' [] ys = init ys
rotate' xs ys = rotate' (tailAll xs) $ ys ++ [headAll xs]

tailAll :: [[b]] -> [[b]]
tailAll xs = [tail x | x <- xs, not (null x)]

headAll :: [[b]] -> [b]
headAll xs = [head x | x <- xs, not (null x)]

-- convert list to list of pairs with occurrences in first list
numberOfOccurrences :: Eq b => [b] -> [(b, Int)]
numberOfOccurrences = numberOfOccurrences' []
  
numberOfOccurrences' :: Eq b => [(b, Int)] -> [b] -> [(b, Int)]
numberOfOccurrences' ys [] = ys
numberOfOccurrences' ys xs = let zs = filter (/= head xs) xs
                             in numberOfOccurrences' ((head xs, length xs - length zs) : ys) zs

splitOn     :: (Char -> Bool) -> String -> [String]
splitOn p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitOn p s''
                            where (w, s'') = break p s'

readFileAsNumberArray :: String -> [[Int]]
readFileAsNumberArray xs = [map digitToInt line | line <- splitOn (=='\n') xs]

readFileAsPairOfStrings :: String -> [(String,String)]
readFileAsPairOfStrings xs = [(head $ splitOn (=='-') line, splitOn (=='-') line !! 1) | line <- splitOn (=='\n') xs]

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

findFirst :: (a -> Bool) -> [a] -> a
findFirst a xs = head (filter a xs)
