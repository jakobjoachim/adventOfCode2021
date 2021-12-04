module Days.Day4 where
  
import Input.Day4Input
import Lib
import Data.List
import Data.Maybe

type  Board = [Row]
type  Row = [Int]

runTask :: Maybe Int
runTask = bingo boards input

runTaskTwo :: Maybe Int
runTaskTwo = bingoTaskTwo boards input

bingo :: [Board] -> [Int] -> Maybe Int
bingo _ [] = Nothing
bingo xs ys = 
  let newBoard = replaceDrawnNumber xs $ head ys
      winningIndex = winningBoardIndex newBoard
  in if isJust winningIndex then Just $ (sumBoard $newBoard !! fromJust winningIndex) * head ys else bingo newBoard $ tail ys
  
bingoTaskTwo :: [Board] -> [Int] -> Maybe Int
bingoTaskTwo _ [] = Nothing
bingoTaskTwo xs ys = 
  let newBoard = replaceDrawnNumber xs $ head ys
      winningIndex = winningBoardIndex newBoard
  in if isJust winningIndex && length xs == 1 then Just $ sumBoard (head newBoard) * head ys else bingoTaskTwo (filter (not . boardHasWon) newBoard) $ tail ys
  
winningBoardIndex :: [Board] -> Maybe Int
winningBoardIndex = findIndex boardHasWon

boardHasWon :: Board -> Bool
boardHasWon xs = any isWinningRow $ xs ++ rotate xs

isWinningRow :: Row -> Bool
isWinningRow x = sum x == (length x * (-1))

sumBoard :: Board -> Int
sumBoard xs = sum [sum $ replace (-1) 0 x | x <- xs]

replaceDrawnNumber :: [Board] -> Int -> [Board]
replaceDrawnNumber xs y = [[replace y (-1) z | z <- x] | x <- xs] 