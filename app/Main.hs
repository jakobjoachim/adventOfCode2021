module Main where

import Days.Day12
import Lib

main :: IO ()
main = do
  content <- readFile "resources/Day12.txt"
  print $ runTask $ readFileAsPairOfStrings content
