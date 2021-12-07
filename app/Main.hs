module Main where

import Days.Day7
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
  content <- readFile "resources/Day7.txt"
  print $ runTask $ readFileAsNumberArray content

readFileAsNumberArray :: String -> [Int]
readFileAsNumberArray = mapMaybe readInt . splitOn (==',')

readInt :: String -> Maybe Int
readInt = readMaybe

splitOn     :: (Char -> Bool) -> String -> [String]
splitOn p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitOn p s''
                            where (w, s'') = break p s'
