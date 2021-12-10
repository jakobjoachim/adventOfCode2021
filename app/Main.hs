module Main where

import Days.Day10
import Lib

main :: IO ()
main = do
  content <- readFile "resources/Day10.txt"
  print $ runTaskTwo $ splitOn (=='\n') content
