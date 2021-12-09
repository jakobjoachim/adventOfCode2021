module Main where

import Days.Day9Task2
import Lib

main :: IO ()
main = do
  content <- readFile "resources/Day9.txt"
  print $ runTask $ readFileAsNumberArray content