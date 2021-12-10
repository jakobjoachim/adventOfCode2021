module Days.Day10 where

runTask :: [String] -> Int
runTask xs = sum [getValue $ foldString x | x <- xs]

foldString :: String -> String
foldString = foldl foldingFunction []
      where   foldingFunction (x:'x':xs) _ = x:'x':xs
              foldingFunction ('(':xs) ')' = xs
              foldingFunction ('<':xs) '>' = xs
              foldingFunction ('{':xs) '}' = xs
              foldingFunction ('[':xs) ']' = xs
              foldingFunction (_:xs) ')' = ')':'x':xs
              foldingFunction (_:xs) '>' = '>':'x':xs
              foldingFunction (_:xs) '}' = '}':'x':xs
              foldingFunction (_:xs) ']' = ']':'x':xs
              foldingFunction xs next = next:xs

getValue :: String -> Int
getValue (')':'x':_) = 3
getValue (']':'x':_) = 57
getValue ('}':'x':_) = 1197
getValue ('>':'x':_) = 25137
getValue _ = 0
