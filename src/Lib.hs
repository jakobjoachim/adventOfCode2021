module Lib (replace, rotate) where

replace :: Eq b => b -> b -> [b] -> [b]
replace a b = map (\x -> if a == x then b else x)

rotate :: Eq b => [[b]] -> [[b]]
rotate xs = rotate' xs []

rotate' :: Eq b => [[b]] -> [[b]] -> [[b]]
rotate' [] ys = init ys
rotate' xs ys = rotate' (tailAll xs) $ ys ++ [headAll xs]

tailAll :: [[b]] -> [[b]]
tailAll xs = [tail x | x <- xs, not (null x)]

headAll :: [[b]] -> [b]
headAll xs = [head x | x <- xs, not (null x)]
