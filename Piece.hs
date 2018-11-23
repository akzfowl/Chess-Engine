module Piece where

import Colour
import PieceType

data Piece = Piece Colour PieceType deriving (Eq)

instance Show Piece where
    show (Piece White p) = "W" ++ show p
    show (Piece Black p) = "B" ++ show p

pieceType :: Piece -> PieceType
pieceType (Piece _ p) = p

pieceColour :: Piece -> Colour
pieceColour (Piece c _) = c