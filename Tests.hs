module Tests where

import Test.HUnit
import Colour
import PieceType
import Piece
import Board
import Game
import Parser
import Move

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
      getPieceTypeInPosition (4,5) initialBoard            ~?= Nothing,
      parseAlternate "O-O"                                 ~?= Just KCastling,
      parseAlternate "O-O-O"                               ~?= Just QCastling,
      parseAlternate "O-X"                                 ~?= Nothing,
      parseAlternate "O-O-"                                ~?= Nothing,
      parseAlternate "o-o"                                 ~?= Nothing,
      parseAlternate "e4"                                  ~?= Just (Normal (Pawn, (4,5), Nothing)),
      parseAlternate "a4"                                  ~?= Just (Normal (Pawn, (4,1), Nothing)),
      parseAlternate "exf5"                                ~?= Just (Normal (Pawn, (5,6), Just 5)),
      parseAlternate "e9"                                  ~?= Nothing,
      parseAlternate "i4"                                  ~?= Nothing,
      parseAlternate "i9"                                  ~?= Nothing,
      parseAlternate "Kb4"                                 ~?= Just (Normal (King, (4,2), Nothing)),
      parseAlternate "Qe7"                                 ~?= Just (Normal (Queen, (7,5), Nothing)),
      parseAlternate "Bd4"                                 ~?= Just (Normal (Bishop, (4,4), Nothing)),
      parseAlternate "Rf4"                                 ~?= Just (Normal (Rook, (4,6), Nothing))
    ]

main :: IO()
main = do _ <- runTestTT boardTests
          return ()
