module Game where

import Colour
import PieceType
import Piece
import Board
import Parser
import Text.Read
import Minmax
import Move

main :: IO()
main = do putStrLn("Which side would you like?(W/B) or E to exit.")
          c <- getLine
          case c of
              co -> case co of
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
              {-runGame (aiMakeMove g (getRandomNextState g 1))-}
              runGame (aiMakeMove g (retrieveNextState g))
           where b = getCurrentBoardFromGame g
                 c = getCurrentColourFromGame g

playerMove :: Game -> IO()
playerMove g  = do putStrLn "Enter your move in standard notation"
                   m <- getLine
                   case m of
                       "" -> do putStrLn "Please enter a valid move"
                                runGame g
                       _  -> case (parseMove m) of
                                    Nothing -> do putStrLn "Move unsuccessful. Please enter a valid move."
                                                  runGame g
                                    Just KCastling -> if canCastleKingSide c b
                                                      then runGame newGame
                                                      else do putStrLn "Move unsuccessful. Please enter a valid move."
                                                              runGame g
                                                      where newGame = castleMove g KCastling
                                    Just QCastling -> if canCastleQueenSide c b
                                                      then runGame newGame
                                                      else do putStrLn "Move unsuccessful. Please enter a valid move."
                                                              runGame g
                                                      where newGame = castleMove g QCastling
                                    Just (Normal (pt, bp)) -> case oldPosition of
                                                                Nothing -> do putStrLn "Move unsuccessful. Please enter a valid move."
                                                                              runGame g
                                                                Just o -> do putStrLn "Move succesful"
                                                                             runGame newGame
                                                                          where newGame = move g o bp
                                                              where b = getCurrentBoardFromGame g
                                                                    c = getCurrentColourFromGame g
                                                                    parserOutput = (parseMove m)
                                                                    --newPosition = snd parserOutput
                                                                    --pieceMoved = fst parserOutput
                                                                    oldPosition = getCurrentPositionBasedOnMove c (pt, bp) b
                                    where b = getCurrentBoardFromGame g
                                          c = getCurrentColourFromGame g

