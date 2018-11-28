module Piece where

import Colour
import PieceType

data Piece = Piece Colour PieceType deriving (Eq)

instance Show Piece where
    show (Piece White p) = {-"W" ++ show p-}
                           case p of
                               King -> "\x265A"
                               Queen -> "\x265B"
                               Rook -> "\x265C"
                               Bishop -> "\x265D"
                               Knight -> "\x265E"
                               Pawn -> "\x265F"
    show (Piece Black p) = {-"B" ++ show p-}
                           case p of
                               King -> "\x2654"
                               Queen -> "\x2655"
                               Rook -> "\x2656"
                               Bishop -> "\x2657"
                               Knight -> "\x2658"
                               Pawn -> "\x2659"

pieceType :: Piece -> PieceType
pieceType (Piece _ p) = p

pieceColour :: Piece -> Colour
pieceColour (Piece c _) = c