module Piece where

import Colour
import PieceType

data Piece = Piece Colour PieceType deriving (Eq)

instance Show Piece where
    show (Piece White p) = "W" ++ show p
    show (Piece Black p) = "B" ++ show p

pointValue :: PieceType -> Int
pointValue pieceType = case pieceType of
    Pawn   -> 1
    Rook   -> 5
    Knight -> 3
    Bishop -> 3
    Queen  -> 9
    King   -> 500