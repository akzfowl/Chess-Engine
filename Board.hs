module Board where

import Colour
import PieceType
import Piece

type Square = Maybe Piece
type Board = [[Square]]
type BoardPosition = (Int, Int)
type HumanReadabalePosition = (Char, Int)

initialBoard :: Board
initialBoard = [[Just(Piece White Rook), Just(Piece White Knight), Just(Piece White Bishop), Just(Piece White Queen), Just(Piece White King), Just(Piece White Bishop), Just(Piece White Knight), Just(Piece White Rook)],
                [Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn)],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                [Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn)],
                [Just(Piece Black Rook), Just(Piece Black Knight), Just(Piece Black Bishop), Just(Piece Black Queen), Just(Piece Black King), Just(Piece Black Bishop), Just(Piece Black Knight), Just(Piece Black Rook)]]

emptyBoard :: Board
emptyBoard = [[Nothing | _ <- [1..8]] | _ <- [1..8]]

getTotalValueOfPlayers :: Board -> (Int, Int)
getTotalValueOfPlayers = foldl compute (0, 0) . concat
                         where compute pts Nothing = pts
                               compute (ptsB, ptsW) (Just (Piece c2 p)) | c2 == Black = (ptsB + (pointValue p), ptsW)
                                                                        | otherwise = (ptsB, ptsW + (pointValue p))