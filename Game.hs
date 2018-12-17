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
main = do putStrLn("What would you like to play? 1 for Player vs Machine or 2 for Player vs Player (or 3 for machine vs machine!)")
          ch <- getLine
          case ch of
              ""  -> do putStrLn "Please enter a valid choice."
                        main
              "1" -> do putStrLn("Which side would you like?(W/B) or E to exit.")
                        c <- getLine
                        case c of
                              co -> case co of
                                          "W" -> do putStrLn("You have chosen to play White")
                                                    runGame initializeBlackAIGame
                                          "B" -> do putStrLn("You have chosen to play Black")
                                                    runGame initializeWhiteAIGame
                                          "E" -> putStrLn("Thanks for playing!")
                                          _   -> do putStrLn("Please enter a valid option")
                                                    main
              "2" -> do putStrLn("White plays first!")
                        runManualGame initializeAILessGame
              "3" -> do putStrLn("White plays first!")
                        runAIGame initializeAILessGame
              _   -> do putStrLn "Please enter a valid choice."
                        main

runManualGame :: Game -> IO()
runManualGame g  = do putStrLn("Current board:")
                      formattedDisplayBoardUsingColour (getCurrentColourFromGame g) (getCurrentBoardFromGame g)
                      if (isCheckMate (getGameState g)) && (getCurrentColourFromGame g) == White
                      then putStrLn "Black has won!"
                      else if (isCheck (getGameState g)) && (getCurrentColourFromGame g) == White
                      then do putStrLn "White is under Check!"
                              playerMoveAlt g True
                      else if (getCurrentColourFromGame g) == White
                      then playerMoveAlt g False
                      else if (isCheckMate (getGameState g)) && (getCurrentColourFromGame g) == Black
                      then putStrLn "White has won!"
                      else if (isCheck (getGameState g)) && (getCurrentColourFromGame g) == Black
                      then do putStrLn "Black is under Check!"
                              playerMoveAlt g True
                      else playerMoveAlt g False
                      
runAIGame :: Game -> IO()
runAIGame g  =     do putStrLn("Current board:")
                      formattedDisplayBoardUsingColour (getCurrentColourFromGame g) (getCurrentBoardFromGame g)
                      if (isCheckMate (getGameState g)) && (getCurrentColourFromGame g) == White
                      then putStrLn "Black has won!"
                      else if (isCheck (getGameState g)) && (getCurrentColourFromGame g) == White
                      then do putStrLn "White is under Check!"
                              aiMoveAlt g
                      else if (getCurrentColourFromGame g) == White
                      then aiMoveAlt g
                      else if (isCheckMate (getGameState g)) && (getCurrentColourFromGame g) == Black
                      then putStrLn "White has won!"
                      else if (isCheck (getGameState g)) && (getCurrentColourFromGame g) == Black
                      then do putStrLn "Black is under Check!"
                              aiMoveAlt g
                      else aiMoveAlt g

runGame :: Game -> IO()
runGame g     = do putStrLn("Current board:")
                   if (getAIColour g) == Black
                   then formattedDisplayBoard1 (getCurrentBoardFromGame g)
                   else formattedDisplayBoard2 (getCurrentBoardFromGame g)
                   if (getAIColour g) == (getCurrentColourFromGame g) && (isCheckMate (getGameState g))
                   then putStrLn "That's Checkmate! You have triumphed over the machine! You must be a talented human!"
                   else if (getAIColour g) == (getCurrentColourFromGame g) && (isCheck (getGameState g))
                   then do putStrLn "The engine is under Check!"
                           aiMove g
                   else if (getAIColour g) == (getCurrentColourFromGame g)
                   then aiMove g
                   else if (isCheckMate (getGameState g))
                   then putStrLn "That's Checkmate! The machine has beaten you! Puny humans shall be cast aside once and for all!"
                   else if (isCheck (getGameState g))
                   then do putStrLn "Check!"
                           playerMove g True
                   else playerMove g False

aiMove :: Game -> IO()
aiMove g = do putStrLn "The engine has made its move"
              displayMoveInNotation (diffStatesToGetMove current nextNew)
              {-runGame (aiMakeMove g (getRandomNextState g 1))-}
              runGame (aiMakeMove g next)
           where b = getCurrentBoardFromGame g
                 c = getCurrentColourFromGame g
                 next = retrieveNextState g
                 current = getGameState g
                 nextNew = (c, getCurrentBoardFromGameState next)

aiMoveAlt :: Game -> IO()
aiMoveAlt g = do putStrLn "The engine has made its move"
                 {-runAIGame (aiMakeMove g (getRandomNextState g 1))-}
                 runAIGame (aiMakeMove g (retrieveNextState g))
                 where b = getCurrentBoardFromGame g
                       c = getCurrentColourFromGame g

playerMove :: Game -> Bool -> IO()
playerMove g isChecked  =  do putStrLn "Enter your move in standard notation"
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
                                                                              Just o ->  if isChecked && isCheck (getGameState checkGame)
                                                                                         then do putStrLn "That move still leaves you in Check. Please enter a valid move."
                                                                                                 runGame g
                                                                                         else do putStrLn "Move succesful"
                                                                                                 runGame newGame
                                                                                    where newGame = move g o bp
                                                                                          checkGame = movePostCheck g o bp
                                                                              where b = getCurrentBoardFromGame g
                                                                                    c = getCurrentColourFromGame g
                                                                                    parserOutput = (parseMove m)
                                                                                    --newPosition = snd parserOutput
                                                                                    --pieceMoved = fst parserOutput
                                                                                    oldPosition = getCurrentPositionBasedOnMove c (pt, bp) b
                                                      where b = getCurrentBoardFromGame g
                                                            c = getCurrentColourFromGame g

playerMoveAlt :: Game -> Bool -> IO()
playerMoveAlt g isChecked  =  do putStrLn "Enter your move in standard notation"
                                 m <- getLine
                                 case m of
                                    "" -> do putStrLn "Please enter a valid move"
                                             runManualGame g
                                    _  -> case (parseMove m) of
                                                      Nothing -> do putStrLn "Move unsuccessful. Please enter a valid move."
                                                                    runManualGame g
                                                      Just KCastling -> if canCastleKingSide c b
                                                                        then runGame newGame
                                                                        else do putStrLn "Move unsuccessful. Please enter a valid move."
                                                                                runManualGame g
                                                                        where newGame = castleMove g KCastling
                                                      Just QCastling -> if canCastleQueenSide c b
                                                                        then runManualGame newGame
                                                                        else do putStrLn "Move unsuccessful. Please enter a valid move."
                                                                                runManualGame g
                                                                        where newGame = castleMove g QCastling
                                                      Just (Normal (pt, bp)) -> case oldPosition of
                                                                              Nothing -> do putStrLn "Move unsuccessful. Please enter a valid move."
                                                                                            runManualGame g
                                                                              Just o ->  if isChecked && isCheck (getGameState newGame)
                                                                                          then do putStrLn "That move still leaves you in Check. Please enter a valid move."
                                                                                                  runManualGame g
                                                                                          else do putStrLn "Move succesful"
                                                                                                  runManualGame newGame
                                                                                    where newGame = move g o bp
                                                                              where b = getCurrentBoardFromGame g
                                                                                    c = getCurrentColourFromGame g
                                                                                    parserOutput = (parseMove m)
                                                                                    --newPosition = snd parserOutput
                                                                                    --pieceMoved = fst parserOutput
                                                                                    oldPosition = getCurrentPositionBasedOnMove c (pt, bp) b
                                                      where b = getCurrentBoardFromGame g
                                                            c = getCurrentColourFromGame g                                                            

