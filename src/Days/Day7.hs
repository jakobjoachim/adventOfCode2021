module Days.Day7 where

runTask :: [Int] ->  Int
runTask input = run input 0 maxBound 

-- input goal currentMin
run :: [Int] -> Int -> Int -> Int
run xs y z = 
  let s = sum [sum [1..(abs(x - y))] | x <- xs] 
  in if s > z then z else run xs (y + 1) s
  