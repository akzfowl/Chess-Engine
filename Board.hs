module Board where

import Colour
import PieceType
import Piece

type Square = Maybe Piece
type Board = [[Square]]
type BoardPosition = (Int, Int)
type HumanReadablePosition = (Char, Int)

boardToHumanReadable :: BoardPosition -> HumanReadablePosition
boardToHumanReadable (p1, p2) = case p1 of
    1 -> ('a',p2)
    2 -> ('b',p2)
    3 -> ('c',p2)
    4 -> ('d',p2)
    5 -> ('e',p2)
    6 -> ('f',p2)
    7 -> ('g',p2)
    8 -> ('h',p2)

humanReadableToBoard :: HumanReadablePosition -> BoardPosition
humanReadableToBoard (p1, p2) = case p1 of
    'a' -> (1,p2)
    'b' -> (2,p2)
    'c' -> (3,p2)
    'd' -> (4,p2)
    'e' -> (5,p2)
    'f' -> (6,p2)
    'g' -> (7,p2)
    'h' -> (8,p2)

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