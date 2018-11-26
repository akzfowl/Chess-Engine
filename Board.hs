module Board where

import Colour
import PieceType
import Piece
import Data.List

type Square = Maybe Piece
type Board = [[Square]]
type BoardPosition = (Int, Int)
type HumanReadablePosition = (Char, Int)

type GameState = (Colour, Board)
type GameHistory = [GameState]
type Game = (GameState, GameHistory)

displayBoard :: Board -> IO()
displayBoard b = putStrLn (unlines (map (concatMap displaySquare) b))

displaySquare :: Square -> String
displaySquare Nothing = "  --   "
displaySquare (Just p) = "  " ++ show p ++ "    "

-- Show white bottom
formattedDisplayBoard1 :: Board -> IO()
formattedDisplayBoard1 b = putStrLn (unlines ((border : boardStr) ++ [border, bottom]))
                           where boardStr = zipWith showLine (reverse [1..8]) $ reverse b
                                 showSquare Nothing  = "  "
                                 showSquare (Just x) = show x ++ " "
                                 border = "  " ++ (replicate 41 '-')
                                 showLine :: Integer -> [Square] -> String
                                 showLine i xs = (intercalate " | " $ (show i) : (map showSquare xs)) ++ " |"
                                 bottom = (intercalate " |  " $ (" " : map (:[])['a'..'h'])) ++ " |"

-- Show white top
formattedDisplayBoard2 :: Board -> IO()
formattedDisplayBoard2 b = putStrLn (unlines ((border : boardStr) ++ [border, bottom]))
                           where boardStr = zipWith showLine ([1..8]) $ b
                                 showSquare Nothing  = "  "
                                 showSquare (Just x) = show x ++ " "
                                 border = "  " ++ (replicate 41 '-')
                                 showLine :: Integer -> [Square] -> String
                                 showLine i xs = (intercalate " | " $ (show i) : (map showSquare xs)) ++ " |"
                                 bottom = (intercalate " |  " $ (" " : map (:[])['a'..'h'])) ++ " |"

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

positions :: [BoardPosition]
positions = [1..8] >>= \p1 -> [1..8] >>= \p2 -> return (p1, p2)

isPositionOnBoard :: BoardPosition -> Bool
isPositionOnBoard (p1, p2) = (p1, p2) `elem` positions

getColourOfPieceInPosition :: BoardPosition -> Board -> Maybe Colour
getColourOfPieceInPosition p b | isPositionOnBoard p = b !! (fst position) !! (snd position) >>= \p -> return (pieceColour p)
                               | otherwise = Nothing
                               where position = (fst p-1, snd p-1)

getPieceTypeInPosition :: BoardPosition -> Board -> Maybe PieceType
getPieceTypeInPosition p b | isPositionOnBoard p = b !! (fst position) !! (snd position) >>= \p -> return (pieceType p)
                           | otherwise = Nothing
                           where position = (fst p-1, snd p-1)

getPieceInPosition :: BoardPosition -> Board -> Maybe Piece
getPieceInPosition p b | isPositionOnBoard p = b !! (fst position) !! (snd position) >>= \p -> return p
                       | otherwise = Nothing
                       where position = (fst p-1, snd p-1)

isPositionEmpty :: BoardPosition -> Board -> Bool
isPositionEmpty p b = case (getColourOfPieceInPosition p b) of
                          Nothing -> True
                          Just _  -> False

isOccupiedByWhitePiece :: BoardPosition -> Board -> Bool
isOccupiedByWhitePiece p b = getColourOfPieceInPosition p b == Just White

isOccupiedByBlackPiece :: BoardPosition -> Board -> Bool
isOccupiedByBlackPiece p b = getColourOfPieceInPosition p b == Just Black

isOccupiedByColour :: Colour -> BoardPosition -> Board -> Bool
isOccupiedByColour c p b = case (getColourOfPieceInPosition p b) of
                           Nothing -> False
                           Just x -> c == x

hasOppositePlayerPiece :: BoardPosition -> BoardPosition -> Board -> Bool
hasOppositePlayerPiece p1 p2 b = if (isOccupiedByBlackPiece p1 b && isOccupiedByWhitePiece p2 b) || (isOccupiedByWhitePiece p1 b && isOccupiedByBlackPiece p2 b)
                                 then True
                                 else False

correctionForMovementDirection :: BoardPosition -> Board -> Int
correctionForMovementDirection p b | isOccupiedByWhitePiece p b = 1
                                   | isOccupiedByBlackPiece p b = -1
                                   | otherwise = 0

moveForwardOnce :: BoardPosition -> Board -> [BoardPosition]
moveForwardOnce (p1, p2) b = [(p1 + 1 * correctionForMovementDirection (p1, p2) b, p2)]

moveForwardTwice :: BoardPosition -> Board -> [BoardPosition]
moveForwardTwice (p1, p2) b | isOccupiedByWhitePiece (p1, p2) b && p1 == 2 = [(p1+2, p2)]
                            | isOccupiedByBlackPiece (p1, p2) b && p2 == 7 = [(p1-2, p2)]
                            | otherwise = []

moveDiagonallyOnceToTheLeft :: BoardPosition -> Board -> [BoardPosition]
moveDiagonallyOnceToTheLeft (p1, p2) b | hasOppositePlayerPiece (p1, p2) (p1 + fst left, p2 + snd left) b = [(p1 + fst left, p2 + snd left)]
                                       | otherwise = []
                                       where left = (1*correctionForMovementDirection (p1, p2) b, -1)

moveDiagonallyOnceToTheRight :: BoardPosition -> Board -> [BoardPosition]
moveDiagonallyOnceToTheRight (p1, p2) b | hasOppositePlayerPiece (p1, p2) (p1 + fst right, p2 + snd right) b = [(p1 + fst right, p2 + snd right)]
                                        | otherwise = []
                                        where right = (1*correctionForMovementDirection (p1, p2) b, 1)

promotePawn :: Colour -> Square
promotePawn c = Just (Piece c Queen)

checkForMoveForwardTwice :: Colour -> BoardPosition -> Board -> Board -> Bool
checkForMoveForwardTwice White (p1, p2) b2 b1 = (isPositionEmpty (p1+2, p2) b2) && (isOccupiedByColour White (p1+2, p2) b1) && (isPositionOccupiedByPawn (p1+2, p2) b1)
checkForMoveForwardTwice Black (p1, p2) b2 b1 = (isPositionEmpty (p1-2, p2) b2) && (isOccupiedByColour Black (p1+2, p2) b1) && (isPositionOccupiedByPawn (p1+2, p2) b1)

isEnPassantMovePossible :: GameState -> GameState -> [BoardPosition]
isEnPassantMovePossible (c2, b2) (c1, b1) | (c1 == White) = filter (\x -> checkForMoveForwardTwice c1 x b2 b1) currentWhiteEnPassantPositions
                                          | (c1 == Black) = filter (\x -> checkForMoveForwardTwice c1 x b2 b1) currentBlackEnPassantPositions
                                          where whiteEnPassantPositions = filter (\x -> not (isPositionEmpty x b2)) [(p1, p2) | p1 <- [4], p2 <- [0..7]]
                                                currentWhiteEnPassantPositions = filter (\x -> isOccupiedByColour c1 x b2 && isPositionOccupiedByPawn x b2) whiteEnPassantPositions
                                                blackEnPassantPositions = filter (\x -> not (isPositionEmpty x b2)) [(p1, p2) | p1 <- [3], p2 <- [0..7]]
                                                currentBlackEnPassantPositions = filter (\x -> isOccupiedByColour c1 x b2 && isPositionOccupiedByPawn x b2) blackEnPassantPositions

hasKingMovedAlready :: Colour -> GameHistory -> Bool
hasKingMovedAlready c gh | c == White = checkPiecePositions c King (0,4) gh
                         | c == Black = checkPiecePositions c King (7,4) gh

hasKingSideRookMovedAlready :: Colour -> GameHistory -> Bool
hasKingSideRookMovedAlready c gh | c == White = checkPiecePositions c Rook (8,1) gh
                                 | c == Black = checkPiecePositions c Rook (8,8) gh

hasQueenSideRookMovedAlready :: Colour -> GameHistory -> Bool
hasQueenSideRookMovedAlready c gh | c == White = checkPiecePositions c Rook (1,1) gh
                                  | c == Black = checkPiecePositions c Rook (1,8) gh

checkPiecePositions :: Colour -> PieceType -> BoardPosition -> GameHistory -> Bool
checkPiecePositions c pt p [] = False
checkPiecePositions c pt p (x:xs) | (getPieceInPosition p b) /= (Just (Piece c pt)) = True
                                  | otherwise = checkPiecePositions c pt p xs
                                  where b = (snd x)

{-checkKingPositions :: Colour -> BoardPosition -> GameHistory -> Bool
checkKingPositions c p [] = False
checkKingPositions c p (x:xs) | (getPieceInPosition p b) /= (Just (Piece c King)) = True
                              | otherwise = checkKingPositions c p xs
                              where b = (snd x)-}

doesPositionResultInCheck :: Colour -> BoardPosition -> Board -> Bool
doesPositionResultInCheck = undefined

canCastleKingSide :: Colour -> Board -> Bool
canCastleKingSide White b = (isHorizontalPathClearBetweenPositions (1,5) (1,8) (1,5) b) && not (any (\x -> doesPositionResultInCheck White x b) [(1,5),(1,6),(1,7)])
canCastleKingSide Black b = (isHorizontalPathClearBetweenPositions (8,5) (8,8) (8,5) b) && not (any (\x -> doesPositionResultInCheck Black x b) [(8,5),(8,5),(8,7)])

canCastleQueenSide :: Colour -> Board -> Bool
canCastleQueenSide White b = (isHorizontalPathClearBetweenPositions (1,5) (1,1) (1,5) b) && not (any (\x -> doesPositionResultInCheck White x b) [(1,5),(1,4),(1,3)])
canCastleQueenSide Black b = (isHorizontalPathClearBetweenPositions (8,5) (8,1) (8,5) b) && not (any (\x -> doesPositionResultInCheck Black x b) [(8,5),(8,4),(8,3)])

castleKingSide :: Colour -> Board -> Board
castleKingSide White b = movePieceBetweenPositions (1,8) (1,6) (movePieceBetweenPositions (1,5) (1,7) b)
castleKingSide Black b = movePieceBetweenPositions (8,8) (8,6) (movePieceBetweenPositions (8,5) (8,7) b)

castleQueenSide :: Colour -> Board -> Board
castleQueenSide White b = movePieceBetweenPositions (1,1) (1,4) (movePieceBetweenPositions (1,5) (1,3) b)
castleQueenSide Black b = movePieceBetweenPositions (8,1) (8,4) (movePieceBetweenPositions (8,5) (8,3) b)

singleStraightMovements :: [BoardPosition]
singleStraightMovements = [(1,0), (0,1), (0,-1), (-1,0)]

singleDiagonalMovements :: [BoardPosition]
singleDiagonalMovements = [(1,1), (-1,1), (1,-1), (-1,-1)]

straightMovements :: [BoardPosition]
straightMovements = singleStraightMovements >>= \n -> [1..8] >>= \p -> return (p * fst n, p * snd n)

diagonalMovements :: [BoardPosition]
diagonalMovements = singleDiagonalMovements >>= \n -> [1..8] >>= \p -> return (p * fst n, p * snd n)

isPathClearBetweenPositions :: BoardPosition -> BoardPosition -> Board -> Bool
isPathClearBetweenPositions p1 p2 b
                {-| (x1 == x2) && (y1 < y2) = isHorizontalPathClearBetweenPositions (x1, y1+1) p2 (x1, y1+1) b
                | (x1 == x2) && (y1 > y2) = isHorizontalPathClearBetweenPositions (x1, y1-1) p2 (x1, y1-1) b
                | (y1 == y2) && (x1 < x2) = isVerticalPathClearBetweenPositions (x1+1, y1) p2 (x1+1, y1) b
                | (y1 == y2) && (x1 > x2) = isVerticalPathClearBetweenPositions (x1-1, y1) p2 (x1-1, y1) b
                | (x2 < x1) && (y2 < y1) = isDiagonalPathClearBetweenPositions (x1-1, y1-1) p2 (x1-1, y1-1) b
                | (x2 < x1) && (y2 > y1) = isDiagonalPathClearBetweenPositions (x1-1, y1+1) p2 (x1-1, y1+1) b
                | (x2 > x1) && (y2 < y1) = isDiagonalPathClearBetweenPositions (x1+1, y1-1) p2 (x1+1, y1-1) b
                | (x2 > x1) && (y2 > y1) = isDiagonalPathClearBetweenPositions (x1+1, y1+1) p2 (x1+1, y1+1) b-}
                | (x1 == x2) && (y1 < y2) = isHorizontalPathClearBetweenPositions p1 p2 p1 b
                | (x1 == x2) && (y1 > y2) = isHorizontalPathClearBetweenPositions p1 p2 p1 b
                | (y1 == y2) && (x1 < x2) = isVerticalPathClearBetweenPositions p1 p2 p1 b
                | (y1 == y2) && (x1 > x2) = isVerticalPathClearBetweenPositions p1 p2 p1 b
                | (x2 < x1) && (y2 < y1) = isDiagonalPathClearBetweenPositions p1 p2 p1 b
                | (x2 < x1) && (y2 > y1) = isDiagonalPathClearBetweenPositions p1 p2 p1 b
                | (x2 > x1) && (y2 < y1) = isDiagonalPathClearBetweenPositions p1 p2 p1 b
                | (x2 > x1) && (y2 > y1) = isDiagonalPathClearBetweenPositions p1 p2 p1 b
                where x1 = fst p1
                      x2 = fst p2
                      y1 = snd p1
                      y2 = snd p2

isHorizontalPathClearBetweenPositions :: BoardPosition -> BoardPosition -> BoardPosition -> Board -> Bool
isHorizontalPathClearBetweenPositions p1 p2 p3 b
                | p1 == p2 = True
                {-| ((isPositionEmpty p2 b) == False) = False-}
                | ((isPositionEmpty p1 b) == False && (p1 /= p3)) = False
                | y2 < y1 = isHorizontalPathClearBetweenPositions (x1,y1-1) p2 p1 b
                | y2 > y1 = isHorizontalPathClearBetweenPositions (x1, y1+1) p2 p1 b
                where x1 = fst p1
                      x2 = fst p2
                      y1 = snd p1
                      y2 = snd p2

isVerticalPathClearBetweenPositions :: BoardPosition -> BoardPosition -> BoardPosition -> Board -> Bool
isVerticalPathClearBetweenPositions p1 p2 p3 b
                | p1 == p2 = True
                {-| ((isPositionEmpty p2 b) == False) = False-}
                | ((isPositionEmpty p1 b) == False && (p1 /= p3)) = False 
                | x2 < x1 = isVerticalPathClearBetweenPositions (x1-1,y1) p2 p1 b
                | x2 > x1 = isVerticalPathClearBetweenPositions (x1+1,y1) p2 p1 b
                where x1 = fst p1
                      x2 = fst p2
                      y1 = snd p1
                      y2 = snd p2

isDiagonalPathClearBetweenPositions :: BoardPosition -> BoardPosition -> BoardPosition -> Board -> Bool
isDiagonalPathClearBetweenPositions p1 p2 p3 b
                | p1 == p2 = True
                {-| ((isPositionEmpty p2 b) == False) = False-}
                | ((isPositionEmpty p1 b) == False && (p1 /= p3)) = False 
                | (x2 < x1) && (y2 < y1) = isDiagonalPathClearBetweenPositions (x1-1,y1-1) p2 p1 b
                | (x2 < x1) && (y2 > y1) = isDiagonalPathClearBetweenPositions (x1-1,y1+1) p2 p1 b
                | (x2 > x1) && (y2 < y1) = isDiagonalPathClearBetweenPositions (x1+1,y1-1) p2 p1 b
                | (x2 > x1) && (y2 > y1) = isDiagonalPathClearBetweenPositions (x1+1,y1+1) p2 p1 b
                where x1 = fst p1
                      x2 = fst p2
                      y1 = snd p1
                      y2 = snd p2

haveOppositeColours :: Piece -> Piece -> Bool
haveOppositeColours (Piece c1 _) (Piece c2 _) = if c1 == c2
                                                then False
                                                else True

isPathClear :: BoardPosition -> BoardPosition -> Board -> Bool
isPathClear p1 p2 b
                | (isPositionEmpty p2 b) == True = isPathClearBetweenPositions p1 p2 b
                | ((isPositionEmpty p2 b) == False) && (hasOppositePlayerPiece p1 p2 b) = isPathClearBetweenPositions p1 p2 b
                | ((isPositionEmpty p2 b) == False) && not (hasOppositePlayerPiece p1 p2 b) = False
                {-where piece1 = getPieceInPosition p1 b
                      piece2 = getPieceInPosition p2 b-}

pawnMovements :: BoardPosition -> Board -> [BoardPosition]
pawnMovements p b = filter (isPositionOnBoard) $ (moveForwardOnce p b) ++ (moveForwardTwice p b) ++ (moveDiagonallyOnceToTheRight p b) ++ (moveDiagonallyOnceToTheLeft p b)

rookMovements :: BoardPosition -> Board -> [BoardPosition]
rookMovements (p1, p2) b = filter (\x -> isPositionOnBoard x && isPathClear (p1, p2) x b) [(p1 + fst s, p2 + snd s) | s <- straightMovements]

knightMovements :: BoardPosition -> Board -> [BoardPosition]
knightMovements (p1, p2) b = filter (\x -> isPositionOnBoard x && (isPositionEmpty x b || hasOppositePlayerPiece (p1,p2) x b)) [(p1+1,p2+2), (p1+1,p2-2), (p1-1,p2+2), (p1-1,p2-2), (p1+2,p2+1), (p1+2,p2-1), (p1-2,p2+1), (p1-2,p2-1)]

bishopMovements :: BoardPosition -> Board -> [BoardPosition]
bishopMovements (p1, p2) b = filter (\x -> isPositionOnBoard x && isPathClear (p1, p2) x b) [(p1 + fst s, p2 + snd s) | s <- diagonalMovements]

queenMovements :: BoardPosition -> Board -> [BoardPosition]
queenMovements (p1, p2) b = filter (\x -> isPositionOnBoard x && isPathClear (p1, p2) x b) [(p1 + fst s, p2 + snd s) | s <- (straightMovements ++ diagonalMovements)]

kingMovements :: BoardPosition -> Board -> [BoardPosition]
kingMovements (p1, p2) b = filter (\x -> isPositionOnBoard x && isPathClear (p1, p2) x b) [(p1 + fst s, p2 + snd s) | s <- (singleStraightMovements ++ singleDiagonalMovements)]

updateBoard :: BoardPosition -> Maybe Piece -> [[Maybe Piece]] -> [[Maybe Piece]]
updateBoard (p1,p2) e (x:xs)
    | u == 0 = updateRow v e x : xs
    | otherwise = x : updateBoard (u,p2) e xs
    where u = p1-1
          v = p2-1

updateRow :: Int -> Maybe Piece -> [Maybe Piece] -> [Maybe Piece]
updateRow 0 e (x:xs) = e:xs
updateRow n e (x:xs) = x : updateRow (n) e xs
updateRow _ _ [] = []

updateBoardUsingPosition :: BoardPosition -> BoardPosition -> Board -> [[Maybe Piece]]
updateBoardUsingPosition p1 p2 b = case (getPieceInPosition p1 b) of
                                   Nothing -> b
                                   x       -> updateBoard p2 x b

updateBoardUsingPositionAndPiece :: BoardPosition -> BoardPosition -> Maybe Piece -> Board -> [[Maybe Piece]]
updateBoardUsingPositionAndPiece p1 p2 p b = case (getPieceInPosition p1 b) of
                                             Nothing -> b
                                             x       -> updateBoard p2 p b

removeFromBoardAtPosition :: BoardPosition -> Board -> Board
removeFromBoardAtPosition p b = case (getPieceInPosition p b) of
                                Nothing -> b
                                Just _  -> updateBoard p Nothing b

isPositionOccupiedByPawn :: BoardPosition -> Board -> Bool
isPositionOccupiedByPawn p1 b = case (getPieceInPosition p1 b) of
                                Nothing             -> False
                                Just (Piece _ Pawn) -> True
                                _                   -> False

movePieceBetweenPositions :: BoardPosition -> BoardPosition -> Board -> Board
movePieceBetweenPositions p1 p2 b =  if (isValidMove p1 p2 b) && (isPositionOccupiedByPawn p1 b) && ((v == 0) || (v == 7)) && (isOccupiedByWhitePiece p1 b)
                                     then removeFromBoardAtPosition p1 (updateBoardUsingPositionAndPiece p1 p2 (promotePawn White) b)
                                     else if (isValidMove p1 p2 b) && (isPositionOccupiedByPawn p1 b) && ((v == 0) || (v == 7)) && (isOccupiedByBlackPiece p1 b)
                                     then removeFromBoardAtPosition p1 (updateBoardUsingPositionAndPiece p1 p2 (promotePawn Black) b)
                                     else if (isValidMove p1 p2 b)
                                     then removeFromBoardAtPosition p1 (updateBoardUsingPosition p1 p2 b)
                                     else b
                                     where v = fst p2

isValidMove :: BoardPosition -> BoardPosition -> Board -> Bool
isValidMove p1 p2 b = case (getPieceInPosition p1 b) of
                          Nothing          -> False
                          Just (Piece c p) -> case p of
                                                  Pawn   -> (p2 `elem` (pawnMovements p1 b))
                                                  Rook   -> (p2 `elem` (rookMovements p1 b))
                                                  Knight -> (p2 `elem` (knightMovements p1 b))
                                                  Bishop -> (p2 `elem` (bishopMovements p1 b))
                                                  Queen  -> (p2 `elem` (queenMovements p1 b))
                                                  King   -> (p2 `elem` (kingMovements p1 b))

initialGameState :: GameState
initialGameState = (White, initialBoard)

initializeGame :: Game
initializeGame = (initialGameState, [])