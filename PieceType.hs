module PieceType where

data PieceType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Eq)

instance Show PieceType where
    show Pawn = "P"
    show Rook = "R"
    show Knight = "N"
    show Bishop = "B"
    show Queen = "Q"
    show King = "K" 

pointValue :: PieceType -> Int
pointValue pieceType = case pieceType of
    Pawn   -> 1
    Rook   -> 5
    Knight -> 3
    Bishop -> 3
    Queen  -> 9
    King   -> 500