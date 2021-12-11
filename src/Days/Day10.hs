module Days.Day10 where
  
import Data.List (sort)

runTaskOne :: [String] -> Int
runTaskOne xs = sum [getValue $ foldString x | x <- xs]

runTaskTwo :: [String] -> Int
runTaskTwo xs = 
   let ys = sort $ map invertBraces $ filter (\x -> (x !! 1) /= 'x') [foldString x | x <- xs]
   in ys !! (length ys `div` 2)

foldString :: String -> String
foldString = foldl foldingFunction []
      where   foldingFunction (x:'x':xs) _ = x:'x':xs
              foldingFunction ('(':xs) ')' = xs
              foldingFunction ('<':xs) '>' = xs
              foldingFunction ('{':xs) '}' = xs
              foldingFunction ('[':xs) ']' = xs
              foldingFunction (  _:xs) ')' = ')':'x':xs
              foldingFunction (  _:xs) '>' = '>':'x':xs
              foldingFunction (  _:xs) '}' = '}':'x':xs
              foldingFunction (  _:xs) ']' = ']':'x':xs
              foldingFunction xs next = next:xs

getValue :: String -> Int
getValue (')':'x':_) = 3
getValue (']':'x':_) = 57
getValue ('}':'x':_) = 1197
getValue ('>':'x':_) = 25137
getValue _ = 0

invertBraces :: [Char] -> Int
invertBraces = foldl foldingFunction 0
                      where   foldingFunction x '(' = x * 5 + 1
                              foldingFunction x '[' = x * 5 + 2
                              foldingFunction x '{' = x * 5 + 3
                              foldingFunction x '<' = x * 5 + 4
                              foldingFunction x _ = x
