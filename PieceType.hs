module PieceType where

data PieceType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Eq)

instance Show PieceType where
    show Pawn = "P"
    show Rook = "R"
    show Knight = "N"
    show Bishop = "B"
    show Queen = "Q"
    show King = "K"