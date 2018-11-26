module Game where

import Colour
import PieceType
import Piece
import Board

main = do
    let b = initialBoard
    startGame b White

startGame :: Board -> Colour -> IO()
startGame b c = undefined{-do
                    putStrLn $ displayBoard b
                    putStrLn "Enter your move"
                    move <- getLine
                    let p-}