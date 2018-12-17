module Parser where

import PieceType
import Data.Char
import Move

-- All the allowed small letters
allowedSmall :: [Char]
allowedSmall = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']

-- All the allowed characters for the pieces
allowedPieces :: [Char]
allowedPieces = ['K', 'Q', 'B', 'N', 'R']

-- A function which converts a character to a piece type
capitalLetterToPieceType :: Char -> PieceType
capitalLetterToPieceType p = case p of
                                 'K' -> King
                                 'Q' -> Queen
                                 'B' -> Bishop
                                 'N' -> Knight
                                 'R' -> Rook
-- A function which converts a small letter to a number on the board
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





-- The main parse function which takes a string and converts it to a Maybe Move
parseAlternate :: String -> Maybe Move
parseAlternate s = if head s == 'O'
                   then parseCastle s
                   else if (head s) `elem` allowedSmall
                        then parsePawnMove s
                        else if (head s) `elem` allowedPieces
                             then parseOtherPieceMove s
                             else if s == "SAVE"
                                  then Just Save
                                  else Nothing





-- Parse a castling move
parseCastle :: String -> Maybe Move
parseCastle s = case s of
                    "O-O" -> Just KCastling
                    "O-O-O" -> Just QCastling
                    otherwise -> Nothing




-- Parse a pawn move
parsePawnMove :: String -> Maybe Move
parsePawnMove (col:x:letter:number:[]) = if (letter `elem` allowedSmall && col `elem` allowedSmall && x == 'x')
                                         then if (number `elem` "12345678")
                                              then Just (Normal (Pawn, (digitToInt number,smallLetterToInt letter), Just (smallLetterToInt col)))
                                              else Nothing
                                         else Nothing

parsePawnMove (letter:number:[]) = if (letter `elem` allowedSmall)
                                   then if (number `elem` ['1'..'8'])
                                        then Just (Normal (Pawn, (digitToInt number, smallLetterToInt letter), Nothing))
                                        else Nothing
                                   else Nothing
parsePawnMove _ = Nothing



-- Parse a move which is not a pawn move
parseOtherPieceMove :: String -> Maybe Move
parseOtherPieceMove (piece:letter:number:[]) = if (piece `elem` allowedPieces)
                                               then if (letter `elem` allowedSmall)
                                                   then if (number `elem` ['1'..'8'])
                                                        then Just (Normal (capitalLetterToPieceType piece, (digitToInt number, smallLetterToInt letter), Nothing))
                                                        else Nothing
                                                   else Nothing
                                               else Nothing
parseOtherPieceMove (piece:attackorcol:letter:number:[]) = if (piece `elem` allowedPieces)
                                                           then if (letter `elem` allowedSmall)
                                                                then if (number `elem` ['1'..'8'])
                                                                     then if attackorcol == 'x'
                                                                          then Just (Normal (capitalLetterToPieceType piece, (digitToInt number, smallLetterToInt letter), Nothing))
                                                                          else if attackorcol `elem` allowedSmall
                                                                               then Just (Normal (capitalLetterToPieceType piece, (digitToInt number, smallLetterToInt letter), Just (smallLetterToInt attackorcol)))
                                                                               else Nothing
                                                                     else Nothing
                                                                else Nothing
                                                           else Nothing
parseOtherPieceMove (piece:col:x:letter:number:[]) = if (piece `elem` allowedPieces && x == 'x')
                                                     then if (letter `elem` allowedSmall && col `elem` allowedSmall)
                                                          then if (number `elem` ['1'..'8'])
                                                               then  Just (Normal (capitalLetterToPieceType piece, (digitToInt number, smallLetterToInt letter), Just (smallLetterToInt col)))
                                                               else Nothing
                                                          else Nothing
                                                     else Nothing

parseOtherPieceMove _ = Nothing






-- Parse a file and return a list of Maybe Moves
-- Even if one of the notations is incorrent, will return Nothing

parseFile :: String ->  IO [Maybe Move]
parseFile f = do something <- readFile f
                 let allParts = words something
                     allMovesString = filter ( \ x -> (not ('.' `elem` x)) ) allParts
                     allMovesConverted = map parseAlternate allMovesString
                 return allMovesConverted










-- OLD STUFF

-- "Rf56789" passes
-- "Rfi" where i > 8 passes
-- Fails when cap and small funcs fail
-- O-O K
-- O-O-O
{- parseMove :: String -> Maybe Move
parseMove s = if head s == 'O'
then case s of
"O-O" -> Just KCastling
"O-O-O" -> Just QCastling
otherwise -> Nothing

else Just (Normal (foldl (\acc x -> if x == 'x' || x == '+'
then acc
else if isLower x && modFst acc == Pawn
then (Pawn, (fst (modSnd acc), smallLetterToInt x), Nothing)
else if isLower x && modFst acc /= Pawn
then (modFst acc, (fst (modSnd acc),smallLetterToInt x), Nothing)
else if isUpper x
then (capitalLetterToPieceType x, modSnd acc, Nothing)
else if isDigit x
then (modFst acc, (digitToInt x, snd (modSnd acc)), Nothing)
else (modFst acc, modSnd acc, Nothing))
(Pawn, (0,0), Nothing) s))



example :: String -> IO()
example f = do l <- parseFile f
case sequence l of
Just l' -> putStrLn $ show l'
Nothing -> return ()



modFst :: (a, b, c) -> a
modFst (x,_,_) = x

modSnd :: (a, b, c) -> b
modSnd (_,y,_) = y

modThd :: (a, b, c) -> c
modThd (_,_,z) = z
-}
