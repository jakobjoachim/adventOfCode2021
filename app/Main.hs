module Main where

import Days.Day8

main :: IO ()
main = do
  content <- readFile "resources/Day8.txt"
  print $ runTask content
