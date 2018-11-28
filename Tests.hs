module Tests where

import Test.HUnit
import Colour
import PieceType
import Piece
import Board
import Game

boardTests :: Test
boardTests = 
    TestList [
      isPathClearBetweenPositions (1,1) (1,6) initialBoard ~?= False,
      isPathClearBetweenPositions (1,3) (1,1) initialBoard ~?= False,
      isPathClearBetweenPositions (7,5) (7,1) initialBoard ~?= False,
      isPathClearBetweenPositions (3,1) (6,1) initialBoard ~?= True,
      isPathClearBetweenPositions (3,1) (5,3) initialBoard ~?= True,
      isPathClearBetweenPositions (5,6) (3,8) initialBoard ~?= True,
      isPositionOccupiedByPiece Rook (1,1) initialBoard    ~?= True,
      isPositionOccupiedByPiece King (1,5) initialBoard    ~?= True,
      isPositionOccupiedByPiece Rook (3,1) initialBoard    ~?= False,
      isPositionOccupiedByPiece Pawn (5,3) initialBoard    ~?= False,
      isPositionOccupiedByPiece Pawn (2,5) initialBoard    ~?= True,
      hasKingMovedAlready White []                         ~?= False,
      getPieceTypeInPosition (1,1) initialBoard            ~?= Just Rook,
      getPieceTypeInPosition (1,4) initialBoard            ~?= Just Queen,
      getPieceTypeInPosition (4,5) initialBoard            ~?= Nothing
    ]

main :: IO()
main = do _ <- runTestTT boardTests
          return ()