module Move where

import PieceType
import Board
import Colour

data Move = Normal (PieceType, BoardPosition, Maybe Int) | KCastling | QCastling | Save deriving (Eq,Show)


castleMove :: Game -> Move -> Game
castleMove g m = (aiColour, (newColour, newBoard), hist ++ [currentState], currentMoveHistory ++ [newMove])
                where   aiColour = getAIColour g
                        currentState = getGameState g
                        hist = getGameHistory g
                        colour = getCurrentColourFromGameState currentState
                        board = getCurrentBoardFromGameState currentState
                        newBoard = if m == KCastling
                                   then castleKingSide colour board
                                   else castleQueenSide colour board
                        newColour = opponent colour
                        newState = (newColour, newBoard)
                        pieceAndPos = diffStatesToGetMove currentState newState
                        newMove = returnMoveInNotation pieceAndPos
                        currentMoveHistory = getMoveHistory g
