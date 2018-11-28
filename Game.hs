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
                                      formattedDisplayBoard1 initialBoard
                                      startGame initializeBlackAIGame White
                            "B" -> do putStrLn("You have chosen to play Black")
                                      formattedDisplayBoard2 initialBoard
                                      startGame initializeWhiteAIGame Black
                            "E" -> putStrLn("Thanks for playing!")
                            _   -> do putStrLn("Please enter a valid option")
                                      main

startGame :: Game -> Colour -> IO()
startGame g c = do putStrLn "Enter your move in standard notation"
                   m <- getLine
                   case readMaybe m of
                       Nothing -> do putStrLn "Please enter a valid move"
                                     startGame g c
                       Just s  -> do putStrLn "Move succesful"
                                     if (getAIColour g) == Black
                                     then formattedDisplayBoard1 newBoard
                                     else formattedDisplayBoard2 newBoard
                                  where b = getCurrentBoardFromGame g
                                        parserOutput = (parseMove s)
                                        newPosition = snd parserOutput
                                        pieceMoved = fst parserOutput
                                        oldPosition = getCurrentPositionBasedOnMove c parserOutput b
                                        newGame = move g oldPosition newPosition
                                        newBoard = getCurrentBoardFromGame newGame

getCurrentPositionBasedOnMove :: Colour -> (PieceType, (Int,Int)) -> Board -> BoardPosition
getCurrentPositionBasedOnMove c (p, (x, y)) b = head $ filter (\a -> isPositionOccupiedByPiece p a b && (x,y) `elem` (getMovementsForPiece p a b)) ownPiecePositions
                                                where ownPiecePositions = filter (\a -> not (isPositionEmpty a b) && isOccupiedByColour c a b) [(u,v) | u <- [1..8], v <- [1..8]]


move :: Game -> BoardPosition -> BoardPosition -> Game
move g bp1 bp2 = (aiColour,(newColour, newBoard), hist ++ [currentState])
                 where aiColour = getAIColour g
                       currentState = getGameState g
                       hist = getGameHistory g
                       colour = getCurrentColourFromGameState currentState
                       board = getCurrentBoardFromGameState currentState
                       newBoard = movePieceBetweenPositions bp1 bp2 board
                       newColour = opponent colour