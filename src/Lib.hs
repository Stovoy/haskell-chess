module Lib (chess) where
import Data.Array

width :: Int
width = 8

height :: Int
height = 8

data State = Empty deriving (Eq, Show)
type Position = (Int, Int)
type Board = Array Position State

initBoard :: Position -> Board
initBoard (width, height) =
  let bounds = ((0, 0), (width - 1, height - 1))
  in array bounds $ zip (range bounds) (repeat Empty)

printArray :: Board -> String
printArray arr =
  unlines [unwords [show (arr ! (x, y)) | x <- [0..width - 1]] | y <- [0..height - 1]]

chess :: IO ()
chess = do
  let board = initBoard (width, height)
  putStrLn $ printArray board
