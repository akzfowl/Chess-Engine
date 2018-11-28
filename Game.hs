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
                                      runGame initializeBlackAIGame
                            "B" -> do putStrLn("You have chosen to play Black")
                                      formattedDisplayBoard2 initialBoard
                                      runGame initializeWhiteAIGame
                            "E" -> putStrLn("Thanks for playing!")
                            _   -> do putStrLn("Please enter a valid option")
                                      main

runGame :: Game -> IO()
runGame g     = do putStrLn("Current board:")
                   if (getAIColour g) == Black
                   then formattedDisplayBoard1 (getCurrentBoardFromGame g)
                   else formattedDisplayBoard2 (getCurrentBoardFromGame g)
                   if (getAIColour g) == (getCurrentColourFromGame g)
                   then aiMove g
                   else playerMove g

aiMove :: Game -> IO()
aiMove g = do putStrLn "The engine has made its move"
              runGame (aiMakeMove g (getRandomNextState g 1))
           where b = getCurrentBoardFromGame g
                 c = getCurrentColourFromGame g

playerMove :: Game -> IO()
playerMove g  = do putStrLn "Enter your move in standard notation"
                   m <- getLine
                   case readMaybe m of
                       Nothing -> do putStrLn "Please enter a valid move"
                                     runGame g
                       Just s  -> do putStrLn "Move succesful"
                                     runGame newGame 
                                  where b = getCurrentBoardFromGame g
                                        c = getCurrentColourFromGame g
                                        parserOutput = (parseMove s)
                                        newPosition = snd parserOutput
                                        pieceMoved = fst parserOutput
                                        oldPosition = getCurrentPositionBasedOnMove c parserOutput b
                                        newGame = move g oldPosition newPosition
                                        newBoard = getCurrentBoardFromGame newGame