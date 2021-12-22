module Main where

import Days.Day14
import Lib

main :: IO ()
main = do
  content <- readFile "resources/Day14.txt"
  print $ runTask content
