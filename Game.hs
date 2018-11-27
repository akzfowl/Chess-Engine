module Game where

import Colour
import PieceType
import Piece
import Board
import Parser
import Text.Read

main :: IO()
main = do putStrLn("Which side would you like?(W/B) Or E to exit.")
          c <- getLine
          case readMaybe c of
              Nothing -> do putStrLn "Please enter a valid option"
                            main
              Just co -> case co of
                            "W" -> do putStrLn("You have chosen to play White")
                                      formattedDisplayBoard2 initialBoard
                                      startGame initialBoard White
                            "B" -> do putStrLn("You have chosen to play Black")
                                      formattedDisplayBoard1 initialBoard
                                      startGame initialBoard Black
                            "E" -> putStrLn("Thanks for playing!")
                            _   -> do putStrLn("Please enter a valid option")
                                      main

startGame :: Board -> Colour -> IO()
startGame b c = undefined{-do putStrLn "Enter your move in standard notation"
                   m <- getLine
                   case readMaybe m of
                       Nothing -> do putStrLn "Please enter a valid move"
                                     startGame b c
                       Just s  -> undefined{-move initializeGame oldPosition newPosition 
                                  where parserOutput = (parseMove s)
                                        newPosition = snd parserOutput
                                        pieceMoved = fst parserOutput
                                        oldPosition = getCurrentPositionBasedOnMove c parserOutput b-}-}

getCurrentPositionBasedOnMove :: Colour -> (PieceType, (Int,Int)) -> Board -> BoardPosition
getCurrentPositionBasedOnMove c (p, (x, y)) b = head $ filter (\a -> isPositionOccupiedByPiece p a b && (x,y) `elem` (getMovementsForPiece p a b)) ownPiecePositions
                                                where ownPiecePositions = filter (\a -> not (isPositionEmpty a b) && isOccupiedByColour c a b) [(u,v) | u <- [1..8], v <- [1..8]]


move :: Game -> BoardPosition -> BoardPosition -> Game
move g bp1 bp2 = undefined{-((newColour, newBoard), hist ++ [currentState])
                 where currentState = fst g
                       hist = snd g
                       colour = fst currentState
                       board = snd currentState
                       newBoard = movePieceBetweenPositions bp1 bp2 board
                       newColour = opponent colour-}