module Lib (chess) where
import Data.Array

width :: Integer
width = 8

height :: Integer
height = 8

data State = Empty |
              WhitePawn | WhiteKnight | WhiteBishop | WhiteRook | WhiteQueen | WhiteKing |
              BlackPawn | BlackKnight | BlackBishop | BlackRook | BlackQueen | BlackKing
              deriving (Eq, Show)
type Position = (Integer, Integer)
type Board = Array Position State

initBoard :: Position -> Board
initBoard (width, height) =
  let bounds = ((0, 0), (width - 1, height - 1))
  in array bounds $ zip (range bounds) [
    BlackRook, BlackBishop, BlackKnight, BlackQueen, BlackKing, BlackBishop, BlackKnight, BlackRook,
    BlackPawn, BlackPawn,   BlackPawn,   BlackPawn,  BlackPawn, BlackPawn,   BlackPawn,   BlackPawn,
    Empty,     Empty,       Empty,       Empty,      Empty,     Empty,       Empty,       Empty,
    Empty,     Empty,       Empty,       Empty,      Empty,     Empty,       Empty,       Empty,
    Empty,     Empty,       Empty,       Empty,      Empty,     Empty,       Empty,       Empty,
    Empty,     Empty,       Empty,       Empty,      Empty,     Empty,       Empty,       Empty,
    WhitePawn, WhitePawn,   WhitePawn,   WhitePawn,  WhitePawn, WhitePawn,   WhitePawn,   WhitePawn,
    WhiteRook, WhiteKnight, WhiteBishop, WhiteQueen, WhiteKing, WhiteBishop, WhiteKnight, WhiteRook]

showPiece :: State -> String
showPiece Empty = " "
showPiece WhitePawn   = "\x1b[37mP\x1b[0m"
showPiece WhiteKnight = "\x1b[37mN\x1b[0m"
showPiece WhiteBishop = "\x1b[37mB\x1b[0m"
showPiece WhiteRook   = "\x1b[37mR\x1b[0m"
showPiece WhiteQueen  = "\x1b[37mQ\x1b[0m"
showPiece WhiteKing   = "\x1b[37mK\x1b[0m"
showPiece BlackPawn   = "\x1b[90mP\x1b[0m"
showPiece BlackKnight = "\x1b[90mN\x1b[0m"
showPiece BlackBishop = "\x1b[90mB\x1b[0m"
showPiece BlackRook   = "\x1b[90mR\x1b[0m"
showPiece BlackQueen  = "\x1b[90mQ\x1b[0m"
showPiece BlackKing   = "\x1b[90mK\x1b[0m"

printArray :: Board -> String
printArray arr =
  unlines [unwords [showPiece (arr ! (y, x)) | x <- [0..width - 1]] | y <- [0..height - 1]]

chess :: IO ()
chess = do
  let board = initBoard (width, height)
  putStrLn $ printArray board
