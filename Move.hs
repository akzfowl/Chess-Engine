module Move where

import PieceType
import Board
import Colour

data Move = Normal (PieceType, BoardPosition) | KCastling | QCastling deriving (Eq)


castleMove :: Game -> Move -> Game
castleMove g m = (aiColour, (newColour, newBoard), hist ++ [currentState])
                where   aiColour = getAIColour g
                        currentState = getGameState g
                        hist = getGameHistory g
                        colour = getCurrentColourFromGameState currentState
                        board = getCurrentBoardFromGameState currentState
                        newBoard = if m == KCastling
                                   then castleKingSide colour board
                                   else castleQueenSide colour board
                        newColour = opponent colour
