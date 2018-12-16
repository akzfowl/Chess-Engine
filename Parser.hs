module Parser where

import PieceType
import Data.Char
import Move

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

-- "Rf56789" passes
-- "Rfi" where i > 8 passes
-- Fails when cap and small funcs fail
-- O-O K
-- O-O-O
parseMove :: String -> Maybe Move
parseMove s = if head s == 'O'
              then case s of
                    "O-O" -> Just KCastling
                    "O-O-O" -> Just QCastling
                    otherwise -> Nothing
              else Just (Normal (foldl (\acc x -> if x == 'x' || x == '+'
                               then acc
                               else if isLower x && fst acc == Pawn
                                    then (Pawn, (fst (snd acc),smallLetterToInt x))
                                    else if isLower x && fst acc /= Pawn
                                         then (fst acc, (fst (snd acc),smallLetterToInt x))
                                         else if isUpper x
                                              then (capitalLetterToPieceType x, snd acc)
                                              else if isDigit x
                                                   then (fst acc, (digitToInt x, snd (snd acc)))
                                                   else (fst acc, snd acc))
                              (Pawn, (0,0)) s))
