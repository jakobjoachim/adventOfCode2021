module Main where

import Days.Day11
import Lib

main :: IO ()
main = do
  content <- readFile "resources/Day11.txt"
  print $ runTaskTwo $ readFileAsNumberArray content
