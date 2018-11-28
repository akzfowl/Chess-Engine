module Parser where

import PieceType
import Data.Char

capitalLetterToPieceType :: Char -> PieceType
capitalLetterToPieceType p = case p of
                                 'K' -> King
                                 'Q' -> Queen
                                 'B' -> Bishop
                                 'N' -> Knight
                                 'R' -> Rook

smallLetterToInt :: Char -> Int
smallLetterToInt p = case p of
                         'a' -> 1
                         'b' -> 2
                         'c' -> 3
                         'd' -> 4
                         'e' -> 5
                         'f' -> 6
                         'g' -> 7
                         'h' -> 8


parseMove :: String -> (PieceType, (Int,Int))
{-parseMove s = foldl (\acc x -> if isLower x then (Pawn, (smallLetterToInt x,snd (snd acc))) else if isUpper x then (capitalLetterToPieceType x, snd acc) else if isDigit x then (fst acc, (fst (snd acc),digitToInt x)) else (fst acc, snd acc)) (Pawn, (0,0)) s-}
parseMove s = foldl (\acc x -> if isLower x && fst acc == Pawn then (Pawn, (fst (snd acc),smallLetterToInt x)) else if isLower x && fst acc /= Pawn then (fst acc, (fst (snd acc),smallLetterToInt x)) else if isUpper x then (capitalLetterToPieceType x, snd acc) else if isDigit x then (fst acc, (digitToInt x, snd (snd acc))) else (fst acc, snd acc)) (Pawn, (0,0)) s