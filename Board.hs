module Board where

import System.Random
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
type Game = (Colour, GameState, GameHistory)


displayBoard :: Board -> IO()
displayBoard b = putStrLn (unlines (map (concatMap displaySquare) b))

displaySquare :: Square -> String
displaySquare Nothing = "  --   "
displaySquare (Just p) = "  " ++ show p ++ "    "

formattedDisplayBoardUsingColour :: Colour -> Board -> IO()
formattedDisplayBoardUsingColour c b = case c of
                                           White -> formattedDisplayBoard1 b
                                           Black -> formattedDisplayBoard2 b

-- Show white bottom
-- White is always at the top, so use reverse on the board
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

-- Functions to get specific portions of the Game and GameState
getAIColour :: Game -> Colour
getAIColour (c, _, _) = c

getGameState :: Game -> GameState
getGameState (_, gs, _) = gs

getGameHistory :: Game -> GameHistory
getGameHistory (_, _, gh) = gh

getCurrentColourFromGame :: Game -> Colour
getCurrentColourFromGame (_, gs, _) = getCurrentColourFromGameState gs

getCurrentBoardFromGame :: Game -> Board
getCurrentBoardFromGame (_, gs, _) = getCurrentBoardFromGameState gs

getCurrentColourFromGameState :: GameState -> Colour
getCurrentColourFromGameState (c, _) = c

getCurrentBoardFromGameState :: GameState -> Board
getCurrentBoardFromGameState (_, b) = b


-- Position convertors (currently not used)
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

-- Initial board for all games
initialBoard :: Board
initialBoard = [[Just(Piece White Rook), Just(Piece White Knight), Just(Piece White Bishop), Just(Piece White Queen), Just(Piece White King), Just(Piece White Bishop), Just(Piece White Knight), Just(Piece White Rook)],
                [Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn)],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                [Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn)],
                [Just(Piece Black Rook), Just(Piece Black Knight), Just(Piece Black Bishop), Just(Piece Black Queen), Just(Piece Black King), Just(Piece Black Bishop), Just(Piece Black Knight), Just(Piece Black Rook)]]

checkBoard :: Board
checkBoard = [[Just(Piece White Rook), Just(Piece White Knight), Just(Piece White Bishop), Just(Piece White Queen), Just(Piece White King), Just(Piece White Bishop), Just(Piece White Knight), Just(Piece White Rook)],
              [Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Nothing, Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn)],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
              [Nothing, Nothing, Nothing, Nothing, Just(Piece Black Queen), Nothing, Nothing , Nothing],
              [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
              [Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn)],
              [Just(Piece Black Rook), Just(Piece Black Knight), Just(Piece Black Bishop), Just(Piece Black Queen), Just(Piece Black King), Just(Piece Black Bishop), Just(Piece Black Knight), Just(Piece Black Rook)]]

checkWithEasySaveBoard :: Board
checkWithEasySaveBoard = [[Just(Piece White Rook), Just(Piece White Knight), Just(Piece White Bishop), Nothing, Just(Piece White King), Nothing, Nothing, Just(Piece White Rook)],
            [Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Nothing, Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn)],
            [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
            [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
            [Nothing, Nothing, Nothing, Nothing, Just(Piece Black Queen), Nothing, Nothing , Nothing],
            [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
            [Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn)],
            [Just(Piece Black Rook), Just(Piece Black Knight), Just(Piece Black Bishop), Just(Piece Black Queen), Just(Piece Black King), Just(Piece Black Bishop), Just(Piece Black Knight), Just(Piece Black Rook)]]

checkWithOneSaveBoard :: Board
checkWithOneSaveBoard = [[Just(Piece White Rook), Just(Piece White Knight), Just(Piece White Bishop), Just(Piece White Rook), Just(Piece White King), Just(Piece White Rook), Just(Piece White Knight), Just(Piece White Rook)],
                  [Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Nothing, Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn)],
                  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                  [Nothing, Nothing, Nothing, Nothing, Just(Piece Black Queen), Nothing, Nothing , Nothing],
                  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                  [Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn)],
                  [Just(Piece Black Rook), Just(Piece Black Knight), Just(Piece Black Bishop), Just(Piece Black Queen), Just(Piece Black King), Just(Piece Black Bishop), Just(Piece Black Knight), Just(Piece Black Rook)]]

checkWithOneCutSaveBoard :: Board
checkWithOneCutSaveBoard = [[Just(Piece White Rook), Just(Piece White Knight), Just(Piece White Bishop), Just(Piece White Rook), Just(Piece White King), Just(Piece White Rook), Nothing, Just(Piece White Rook)],
                  [Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Nothing, Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn)],
                  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                  [Nothing, Nothing, Nothing, Nothing, Just(Piece Black Queen), Nothing, Nothing , Just(Piece White Rook)],
                  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                  [Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn)],
                  [Just(Piece Black Rook), Just(Piece Black Knight), Just(Piece Black Bishop), Just(Piece Black Queen), Just(Piece Black King), Just(Piece Black Bishop), Just(Piece Black Knight), Just(Piece Black Rook)]]

checkmateBoard :: Board
checkmateBoard = [[Just(Piece White Rook), Just(Piece White Knight), Just(Piece White Bishop), Just(Piece White Rook), Just(Piece White King), Just(Piece White Rook), Nothing, Just(Piece White Rook)],
                  [Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Nothing, Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn)],
                  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                  [Nothing, Nothing, Nothing, Nothing, Just(Piece Black Queen), Nothing, Nothing , Nothing],
                  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                  [Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn)],
                  [Just(Piece Black Rook), Just(Piece Black Knight), Just(Piece Black Bishop), Just(Piece Black Queen), Just(Piece Black King), Just(Piece Black Bishop), Just(Piece Black Knight), Just(Piece Black Rook)]]

checkmateBoard2 :: Board
checkmateBoard2 = [[Just(Piece White Rook), Just(Piece White Knight), Just(Piece White Bishop), Nothing, Just(Piece White King), Nothing, Just(Piece White Knight), Just(Piece White Rook)],
                  [Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Nothing, Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn)],
                  [Nothing, Nothing, Nothing, Nothing, Just(Piece White Pawn), Nothing, Nothing , Nothing],
                  [Nothing, Nothing, Just(Piece White Bishop), Nothing, Nothing, Nothing, Nothing , Nothing],
                  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                  [Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece White Queen), Just(Piece Black Pawn), Just(Piece Black Pawn)],
                  [Just(Piece Black Rook), Just(Piece Black Knight), Just(Piece Black Bishop), Just(Piece Black Queen), Just(Piece Black King), Just(Piece Black Bishop), Just(Piece Black Rook), Nothing]]

kingCannotCaptureBoard :: Board
kingCannotCaptureBoard = [[Just(Piece White Rook), Just(Piece White Knight), Just(Piece White Bishop), Just(Piece White Queen), Just(Piece White King), Nothing, Nothing, Just(Piece White Rook)],
                  [Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Nothing, Just(Piece Black Knight), Just(Piece White Pawn), Just(Piece White Pawn)],
                  [Nothing, Nothing, Nothing, Nothing, Just(Piece White Pawn), Just(Piece White Knight), Nothing , Nothing],
                  [Nothing, Nothing, Just(Piece White Bishop), Nothing, Nothing, Nothing, Nothing , Just(Piece Black Queen)],
                  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                  [Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn)],
                  [Just(Piece Black Rook), Just(Piece Black Knight), Just(Piece Black Bishop), Nothing, Just(Piece Black King), Just(Piece Black Bishop), Nothing, Just(Piece Black Rook)]]

kingCanCaptureBoard :: Board
kingCanCaptureBoard = [[Just(Piece White Rook), Just(Piece White Knight), Just(Piece White Bishop), Just(Piece White Queen), Just(Piece White King), Nothing, Nothing, Just(Piece White Rook)],
                  [Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Nothing, Just(Piece Black Knight), Just(Piece White Pawn), Just(Piece White Pawn)],
                  [Nothing, Nothing, Nothing, Nothing, Just(Piece White Pawn), Just(Piece White Knight), Nothing , Nothing],
                  [Nothing, Nothing, Just(Piece White Bishop), Nothing, Nothing, Nothing, Nothing , Nothing],
                  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                  [Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn)],
                  [Just(Piece Black Rook), Just(Piece Black Knight), Just(Piece Black Bishop), Just(Piece Black Queen), Just(Piece Black King), Just(Piece Black Bishop), Nothing, Just(Piece Black Rook)]]

kingCanCaptureWithThreatBoard :: Board
kingCanCaptureWithThreatBoard = [[Just(Piece White Rook), Just(Piece White Knight), Just(Piece White Bishop), Just(Piece White Queen), Just(Piece White King), Nothing, Nothing, Just(Piece White Rook)],
                  [Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Nothing, Just(Piece Black Knight), Just(Piece White Pawn), Just(Piece White Pawn)],
                  [Nothing, Nothing, Nothing, Nothing, Just(Piece White Pawn), Just(Piece White Knight), Nothing , Nothing],
                  [Nothing, Nothing, Just(Piece Black Queen), Nothing, Nothing, Nothing, Nothing , Nothing],
                  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                  [Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn)],
                  [Just(Piece Black Rook), Just(Piece Black Knight), Just(Piece Black Bishop), Just(Piece Black Queen), Just(Piece Black King), Just(Piece Black Bishop), Nothing, Just(Piece Black Rook)]]

-- Empty board in case needed
emptyBoard :: Board
emptyBoard = [[Nothing | _ <- [1..8]] | _ <- [1..8]]

-- Primitive evaluator of both players' values based on the values ascribed to their pieces.
getTotalValueOfPlayers :: Board -> (Int, Int)
getTotalValueOfPlayers = foldl compute (0, 0) . concat
                         where compute pts Nothing = pts
                               compute (ptsB, ptsW) (Just (Piece c2 p)) | c2 == Black = (ptsB + (pointValue p), ptsW)
                                                                        | otherwise = (ptsB, ptsW + (pointValue p))

positions :: [BoardPosition]
positions = [1..8] >>= \p1 -> [1..8] >>= \p2 -> return (p1, p2)

-- Position validator
isPositionOnBoard :: BoardPosition -> Bool
isPositionOnBoard (p1, p2) = (p1, p2) `elem` positions

-- Query square functions
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

-- Colour queries
isOccupiedByWhitePiece :: BoardPosition -> Board -> Bool
isOccupiedByWhitePiece p b = getColourOfPieceInPosition p b == Just White

isOccupiedByBlackPiece :: BoardPosition -> Board -> Bool
isOccupiedByBlackPiece p b = getColourOfPieceInPosition p b == Just Black

isOccupiedByColour :: Colour -> BoardPosition -> Board -> Bool
isOccupiedByColour c p b = case (getColourOfPieceInPosition p b) of
                           Nothing -> False
                           Just x -> c == x

-- Player query
hasOppositePlayerPiece :: BoardPosition -> BoardPosition -> Board -> Bool
hasOppositePlayerPiece p1 p2 b = if (isOccupiedByBlackPiece p1 b && isOccupiedByWhitePiece p2 b) || (isOccupiedByWhitePiece p1 b && isOccupiedByBlackPiece p2 b)
                                 then True
                                 else False

--Movement correction for pawns
correctionForMovementDirection :: BoardPosition -> Board -> Int
correctionForMovementDirection p b | isOccupiedByWhitePiece p b = 1
                                   | isOccupiedByBlackPiece p b = -1
                                   | otherwise = 0

--Pawn movement helpers
moveForwardOnce :: BoardPosition -> Board -> [BoardPosition]
{-moveForwardOnce (p1, p2) b = [(p1 + 1 * correctionForMovementDirection (p1, p2) b, p2)]-}
moveForwardOnce (p1, p2) b | isOccupiedByWhitePiece (p1, p2) b && (isPositionEmpty (p1+1, p2) b) = [(p1+1, p2)]
                           | isOccupiedByBlackPiece (p1, p2) b && (isPositionEmpty (p1-1, p2) b) = [(p1-1, p2)]
                           | otherwise = []

moveForwardTwice :: BoardPosition -> Board -> [BoardPosition]
moveForwardTwice (p1, p2) b | isOccupiedByWhitePiece (p1, p2) b && (isPositionEmpty (p1+2, p2) b) && (isPositionEmpty (p1+1, p2) b) && p1 == 2 = [(p1+2, p2)]
                            | isOccupiedByBlackPiece (p1, p2) b && (isPositionEmpty (p1-2, p2) b) && (isPositionEmpty (p1-1, p2) b) && p1 == 7 = [(p1-2, p2)]
                            | otherwise = []

moveDiagonallyOnceToTheLeft :: BoardPosition -> Board -> [BoardPosition]
moveDiagonallyOnceToTheLeft (p1, p2) b | hasOppositePlayerPiece (p1, p2) (p1 + fst left, p2 + snd left) b = [(p1 + fst left, p2 + snd left)]
                                       | otherwise = []
                                       where left = (1*correctionForMovementDirection (p1, p2) b, -1)

moveDiagonallyOnceToTheRight :: BoardPosition -> Board -> [BoardPosition]
moveDiagonallyOnceToTheRight (p1, p2) b | hasOppositePlayerPiece (p1, p2) (p1 + fst right, p2 + snd right) b = [(p1 + fst right, p2 + snd right)]
                                        | otherwise = []
                                        where right = (1*correctionForMovementDirection (p1, p2) b, 1)

-- Special case (need to add all options)
promotePawn :: Colour -> Square
promotePawn c = Just (Piece c Queen)

-- Special case for pawns
checkForMoveForwardTwice :: Colour -> BoardPosition -> Board -> Board -> Bool
checkForMoveForwardTwice White (p1, p2) b2 b1 = (isPositionEmpty (p1+2, p2) b2) && (isOccupiedByColour White (p1+2, p2) b1) && (isPositionOccupiedByPawn (p1+2, p2) b1)
checkForMoveForwardTwice Black (p1, p2) b2 b1 = (isPositionEmpty (p1-2, p2) b2) && (isOccupiedByColour Black (p1+2, p2) b1) && (isPositionOccupiedByPawn (p1+2, p2) b1)

-- Another special case
isEnPassantMovePossible :: GameState -> GameState -> [BoardPosition]
isEnPassantMovePossible (c2, b2) (c1, b1) | (c1 == White) = filter (\x -> checkForMoveForwardTwice c1 x b2 b1) currentWhiteEnPassantPositions
                                          | (c1 == Black) = filter (\x -> checkForMoveForwardTwice c1 x b2 b1) currentBlackEnPassantPositions
                                          where whiteEnPassantPositions = filter (\x -> not (isPositionEmpty x b2)) [(p1, p2) | p1 <- [4], p2 <- [0..7]]
                                                currentWhiteEnPassantPositions = filter (\x -> isOccupiedByColour c1 x b2 && isPositionOccupiedByPawn x b2) whiteEnPassantPositions
                                                blackEnPassantPositions = filter (\x -> not (isPositionEmpty x b2)) [(p1, p2) | p1 <- [3], p2 <- [0..7]]
                                                currentBlackEnPassantPositions = filter (\x -> isOccupiedByColour c1 x b2 && isPositionOccupiedByPawn x b2) blackEnPassantPositions

enPassantMove :: GameState -> BoardPosition -> [Board]
enPassantMove (c1, b1) (p1, p2)
                | (c1 == White) = [movePieceBetweenPositions pos (p1-1, p2) (removeFromBoardAtPosition (p1, p2) b1) | pos <- pawnPositions]
                | (c1 == Black) = [movePieceBetweenPositions pos (p1+1, p2) (removeFromBoardAtPosition (p1, p2) b1) | pos <- pawnPositions]
                where pawnPositions = filter (\x -> isOccupiedByColour c1 x b1 && isPositionOccupiedByPawn x b1) [(i,j) | i <- [p1], j <- [p2-1,p2+1], j >= 1, j<= 8]

-- Checks for castling
hasKingMovedAlready :: Colour -> GameHistory -> Bool
hasKingMovedAlready c gh | c == White = checkPiecePositions c King (0,4) gh
                         | c == Black = checkPiecePositions c King (7,4) gh

hasKingSideRookMovedAlready :: Colour -> GameHistory -> Bool
hasKingSideRookMovedAlready c gh | c == White = checkPiecePositions c Rook (8,1) gh
                                 | c == Black = checkPiecePositions c Rook (8,8) gh

hasQueenSideRookMovedAlready :: Colour -> GameHistory -> Bool
hasQueenSideRookMovedAlready c gh | c == White = checkPiecePositions c Rook (1,1) gh
                                  | c == Black = checkPiecePositions c Rook (1,8) gh

-- Checks if any movement is present in game history
checkPiecePositions :: Colour -> PieceType -> BoardPosition -> GameHistory -> Bool
checkPiecePositions c pt p [] = False
checkPiecePositions c pt p (x:xs) | (getPieceInPosition p b) /= (Just (Piece c pt)) = True
                                  | otherwise = checkPiecePositions c pt p xs
                                  where b = (snd x)

-- Checking for checks
doesPositionResultInCheck :: BoardPosition -> BoardPosition -> Board -> Bool
doesPositionResultInCheck kingP p b = kingP `elem` (generateLegalMoves p b)


-- Castling validators
canCastleKingSide :: Colour -> Board -> Bool
canCastleKingSide White b = (isHorizontalPathClearBetweenPositions (1,5) (1,8) (1,5) b) && not (isUnderCheck (1,5) (1,6) b) && not (isUnderCheck (1,5) (1,7) b)
canCastleKingSide Black b = (isHorizontalPathClearBetweenPositions (8,5) (8,8) (8,5) b) && not (isUnderCheck (8,5) (8,6) b) && not (isUnderCheck (8,5) (8,7) b)

canCastleQueenSide :: Colour -> Board -> Bool
canCastleQueenSide White b = (isHorizontalPathClearBetweenPositions (1,5) (1,1) (1,5) b) && not (isUnderCheck (1,5) (1,4) b) && not (isUnderCheck (1,5) (1,3) b)
canCastleQueenSide Black b = (isHorizontalPathClearBetweenPositions (8,5) (8,1) (8,5) b) && not (isUnderCheck (8,5) (8,4) b) && not (isUnderCheck (8,5) (8,3) b)

castleKingSide :: Colour -> Board -> Board
castleKingSide White b = movePieceBetweenPositionsSpl (1,8) (1,6) (movePieceBetweenPositionsSpl (1,5) (1,7) b)
castleKingSide Black b = movePieceBetweenPositionsSpl (8,8) (8,6) (movePieceBetweenPositionsSpl (8,5) (8,7) b)

castleQueenSide :: Colour -> Board -> Board
castleQueenSide White b = movePieceBetweenPositionsSpl (1,1) (1,4) (movePieceBetweenPositionsSpl (1,5) (1,3) b)
castleQueenSide Black b = movePieceBetweenPositionsSpl (8,1) (8,4) (movePieceBetweenPositionsSpl (8,5) (8,3) b)

castling :: Bool -> Bool -> Bool -> Colour -> Board -> [Board]
castling True _ _ _ _ = []
castling False False False c b
                | p1 && p2  = [castleKingSide c b] ++ [castleQueenSide c b]
                | p1        = [castleKingSide c b]
                | p2        = [castleQueenSide c b]
                | otherwise = []
                where p1 = canCastleKingSide c b
                      p2 = canCastleQueenSide c b
castling False True False c b
                | p2        = [castleQueenSide c b]
                | otherwise = []
                where p1 = canCastleKingSide c b
                      p2 = canCastleQueenSide c b
castling False False True c b
                | p1        = [castleKingSide c b]
                | otherwise = []
                where p1 = canCastleKingSide c b
                      p2 = canCastleQueenSide c b

movePieceBetweenPositionsSpl :: BoardPosition -> BoardPosition -> Board -> Board
movePieceBetweenPositionsSpl p1 p2 b =  removeFromBoardAtPosition p1 (updateBoardUsingPosition p1 p2 b)

-- Movement helpers
singleStraightMovements :: [BoardPosition]
singleStraightMovements = [(1,0), (0,1), (0,-1), (-1,0)]

singleDiagonalMovements :: [BoardPosition]
singleDiagonalMovements = [(1,1), (-1,1), (1,-1), (-1,-1)]

straightMovements :: [BoardPosition]
straightMovements = singleStraightMovements >>= \n -> [1..8] >>= \p -> return (p * fst n, p * snd n)

diagonalMovements :: [BoardPosition]
diagonalMovements = singleDiagonalMovements >>= \n -> [1..8] >>= \p -> return (p * fst n, p * snd n)


-- Movement validators
isPathClearBetweenPositions :: BoardPosition -> BoardPosition -> Board -> Bool
isPathClearBetweenPositions p1 p2 b
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
                | otherwise              = False
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
      | (isPositionEmpty p2 b) == True && (not (isPositionOccupiedByPiece King p1 b)) = isPathClearBetweenPositions p1 p2 b
      | (isPositionEmpty p2 b) == True && (isPositionOccupiedByPiece King p1 b) && ((getColourOfPieceInPosition p1 b) == Just White) && not (isPieceUnderAttack Black p2 interimBoard) = isPathClearBetweenPositions p1 p2 b
      | (isPositionEmpty p2 b) == True && (isPositionOccupiedByPiece King p1 b) && ((getColourOfPieceInPosition p1 b) == Just Black) && not (isPieceUnderAttack White p2 interimBoard) = isPathClearBetweenPositions p1 p2 b
      | ((isPositionEmpty p2 b) == False) && (hasOppositePlayerPiece p1 p2 b) && (not (isPositionOccupiedByPiece King p1 b)) = isPathClearBetweenPositions p1 p2 b 
      | ((isPositionEmpty p2 b) == False) && (hasOppositePlayerPiece p1 p2 b) && (isPositionOccupiedByPiece King p1 b) && ((getColourOfPieceInPosition p1 b) == Just White) && not (isPieceUnderAttack Black p2 interimBoard) = isPathClearBetweenPositions p1 p2 b
      | ((isPositionEmpty p2 b) == False) && (hasOppositePlayerPiece p1 p2 b) && (isPositionOccupiedByPiece King p1 b) && ((getColourOfPieceInPosition p1 b) == Just Black) && not (isPieceUnderAttack White p2 interimBoard) = isPathClearBetweenPositions p1 p2 b
      | ((isPositionEmpty p2 b) == False) && not (hasOppositePlayerPiece p1 p2 b) = False
      | otherwise = False
      where interimBoard = moveUnsafe p1 p2 b



-- Get all possible movements for a specific piece from a given position
getMovementsForPiece :: PieceType -> BoardPosition -> Board -> [BoardPosition]
getMovementsForPiece p bp b = case p of
                                  Pawn   -> pawnMovements bp b
                                  Rook   -> rookMovements bp b
                                  Knight -> knightMovements bp b
                                  Bishop -> bishopMovements bp b
                                  Queen  -> queenMovements bp b
                                  King   -> kingMovements bp b

-- Generate all movements for a specific piece from a position
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

-- Validator
isValidPosition :: BoardPosition -> Bool
isValidPosition (p1, p2) = if(p1 < 1 || p1 > 8 || p2 < 1 || p2 > 8)
                           then False
                           else True

-- Base functions used to manipulate the board
updateBoard :: BoardPosition -> Maybe Piece -> [[Maybe Piece]] -> [[Maybe Piece]]
updateBoard (p1,p2) e (x:xs)
    | u == 0 = updateRow v e x : xs
    | otherwise = x : updateBoard (u,p2) e xs
    where u = p1-1
          v = p2-1

updateRow :: Int -> Maybe Piece -> [Maybe Piece] -> [Maybe Piece]
updateRow 0 e (x:xs) = e:xs
updateRow n e (x:xs) = x : updateRow (n-1) e xs
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

-- Piece checkers
isPositionOccupiedByPiece :: PieceType -> BoardPosition -> Board -> Bool
isPositionOccupiedByPiece p bp b = case (getPieceInPosition bp b) of
                                   Nothing          -> False
                                   Just (Piece _ c) -> c == p

isPositionOccupiedByPawn :: BoardPosition -> Board -> Bool
isPositionOccupiedByPawn p1 b = case (getPieceInPosition p1 b) of
                                Nothing             -> False
                                Just (Piece _ Pawn) -> True
                                _                   -> False

isPositionOccupiedByKing :: BoardPosition -> Board -> Bool
isPositionOccupiedByKing p1 b = case (getPieceInPosition p1 b) of
                                Nothing             -> False
                                Just (Piece _ King) -> True
                                _                   -> False                               

-- Actual movement between positions
movePieceBetweenPositions :: BoardPosition -> BoardPosition -> Board -> Board
movePieceBetweenPositions p1 p2 b =  if (isValidMove p1 p2 b) && (isPositionOccupiedByPawn p1 b) && (v == 8) && (isOccupiedByWhitePiece p1 b)
                                     then removeFromBoardAtPosition p1 (updateBoardUsingPositionAndPiece p1 p2 (promotePawn White) b)
                                     else if (isValidMove p1 p2 b) && (isPositionOccupiedByPawn p1 b) && (v == 1) && (isOccupiedByBlackPiece p1 b)
                                          then removeFromBoardAtPosition p1 (updateBoardUsingPositionAndPiece p1 p2 (promotePawn Black) b)
                                          else if (isValidMove p1 p2 b)
                                               then removeFromBoardAtPosition p1 (updateBoardUsingPosition p1 p2 b)
                                               else b
                                     where v = fst p2

moveUnsafe :: BoardPosition -> BoardPosition -> Board -> Board
moveUnsafe p1 p2 b =  removeFromBoardAtPosition p1 (updateBoardUsingPosition p1 p2 b)

-- Check validaity of moves for all pieces
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

isUnderCheck :: BoardPosition -> BoardPosition -> Board -> Bool
isUnderCheck p1 p2 bp = case (getColourOfPieceInPosition p1 bp) of
                            Nothing  -> False
                            Just c   -> if (null (checkPositions c (movePieceBetweenPositions p1 p2 bp)))
                                        then False
                                        else True

getPositionOfColouredKing :: Colour -> Board -> BoardPosition 
getPositionOfColouredKing c b = head (filter (\x -> isPositionOccupiedByPiece King x b) (getAllPositionsOfColourPieces c b))

isCheck :: GameState -> Bool
isCheck (c, b) = isPieceUnderAttack (opponent c) (getPositionOfColouredKing c b) b

canBeIntercepted :: Colour -> BoardPosition -> Board -> Bool
canBeIntercepted c bp b = if snd attackInfo == Knight
                          then canTakeAttackingPiece
                          else (canTakeAttackingPiece || isBlockerAvailable)
                          where attackInfo = getPositionOfAttackerWithPieceType (opponent c) bp b
                                kingPosition = getPositionOfColouredKing c b
                                attackerPosition = fst attackInfo
                                allInterimPositions =  getAllPositionsBetweenPositions kingPosition attackerPosition
                                canTakeAttackingPiece = isPieceUnderAttack c (fst attackInfo) (removeFromBoardAtPosition (attackerPosition) b)
                                ownPiecePositions p = filter (\a -> not (isPositionEmpty a b) && isOccupiedByColour c a b && isPositionOccupiedByPiece p a b) [(u,v) | u <- [1..8], v <- [1..8]]
                                ownPieceMoves = concat (concat (generateBoardMovesForColour b c))
                                blockerMoves = intersect allInterimPositions ownPieceMoves
                                isBlockerAvailable = if (length blockerMoves) > 0 then True else False

                                    {-getMovementsForPiece :: PieceType -> BoardPosition -> Board -> [BoardPosition]-}
                                    {-generateBoardMoves :: Board -> [[[BoardPosition]]]-}
{-canCutAttacker :: Colour -> BoardPosition -> Board -> Bool
canCutAttacker-}

getAllPositionsBetweenPositions :: BoardPosition -> BoardPosition -> [BoardPosition]
getAllPositionsBetweenPositions bp1 bp2 = if x1 == x2 && y1 < y2
                                          then [(i,j) | i <- [x1], j <- [y1+1..y2-1]]
                                          else if x1 == x2 && y1 > y2
                                          then [(i,j) | i <- [x1], j <- [y2+1..y1-1]]
                                          else if y1 == y2 && x1 < x2
                                          then [(i,j) | j <- [y1], i <- [x1+1..x2-1]]
                                          else [(i,j) | j <- [y1], i <- [x2+1..x1-1]]
                                          where x1 = fst bp1
                                                y1 = snd bp1
                                                x2 = fst bp2
                                                y2 = snd bp2

isCheckMate :: GameState -> Bool
isCheckMate (c, b) = (isPieceUnderAttack (opponent c) p b) && (lKMoves == lKUnderAttack) && (not (canBeIntercepted c p b))
                     where p = getPositionOfColouredKing c b
                           kMoves = kingMovements p b
                           lKMoves = length kMoves
                           lKUnderAttack = length (filter (\x -> isPieceUnderAttack (opponent c) x b) (kMoves))

isPieceUnderAttack :: Colour -> BoardPosition -> Board -> Bool
isPieceUnderAttack c bp b = if (isPieceAttackingPosition Pawn c bp b) || (isPieceAttackingPosition Rook c bp b) || (isPieceAttackingPosition Knight c bp b) || (isPieceAttackingPosition Bishop c bp b) || (isPieceAttackingPosition Queen c bp b)
                           then True
                           else False

getPositionOfAttackerWithPieceType :: Colour -> BoardPosition -> Board -> (BoardPosition, PieceType)
getPositionOfAttackerWithPieceType c bp b = if (fst pawnCheck)
                                            then (snd pawnCheck, Pawn)
                                            else if (fst rookCheck)
                                            then (snd rookCheck, Rook)
                                            else if (fst knightCheck)
                                            then (snd knightCheck, Knight)
                                            else if (fst bishopCheck)
                                            then (snd bishopCheck, Bishop)
                                            else if (fst queenCheck)
                                            then (snd queenCheck, Queen)
                                            else (snd kingCheck, King)
                                            where pawnCheck   = isPieceAttackingPositionWithPosition Pawn c bp b
                                                  rookCheck   = isPieceAttackingPositionWithPosition Rook c bp b
                                                  knightCheck = isPieceAttackingPositionWithPosition Knight c bp b
                                                  bishopCheck = isPieceAttackingPositionWithPosition Bishop c bp b
                                                  queenCheck  = isPieceAttackingPositionWithPosition Queen c bp b
                                                  kingCheck   = isPieceAttackingPositionWithPosition King c bp b

isPieceAttackingPosition :: PieceType -> Colour -> BoardPosition -> Board -> Bool
isPieceAttackingPosition pt c bp b = if length (l) > 0
                                     then True
                                     else False
                                     where l = (filter (\x -> isValidMove x bp b) (filter (\x -> isPositionOccupiedByPiece pt x b) (getAllPositionsOfColourPieces c b)))                               

{-isValidMove :: BoardPosition -> BoardPosition -> Board -> Bool-}

isPieceAttackingPositionWithPosition :: PieceType -> Colour -> BoardPosition -> Board -> (Bool, BoardPosition)
isPieceAttackingPositionWithPosition pt c bp b = if length (l) > 0
                                                 then (True, (head l))
                                                 else (False, (0,0))
                                                 where l = (filter (\x -> isValidMove x bp b) (filter (\x -> isPositionOccupiedByPiece pt x b) (getAllPositionsOfColourPieces c b)))

getAllPositionsOfColourPieces :: Colour -> Board -> [BoardPosition]
getAllPositionsOfColourPieces c b = filter (\x -> not (isPositionEmpty x b) && isOccupiedByColour c x b) [(u,v) | u <- [1..8], v <- [1..8]]

-- Retrieve all possible game states where checks are possible
checkPositions :: Colour -> Board -> [BoardPosition]
checkPositions c b = filter (\x -> doesPositionResultInCheck kingPosition x b) [positions | positions <- opponentPiecePositions]
                     where opponentPiecePositions = filter (\x -> not (isPositionEmpty x b) && isOccupiedByColour (opponent c) x b) [(u,v) | u <- [1..8], v <- [1..8]]
                           ownPiecePositions      = filter (\x -> not (isPositionEmpty x b) && isOccupiedByColour c x b) [(u,v) | u <- [1..8], v <- [1..8]]
                           kingPosition           = head (filter (\x -> isPositionOccupiedByKing x b) [positions | positions <- ownPiecePositions])

generateBoardMoves :: Board -> [[[BoardPosition]]]
generateBoardMoves b = [map (validatePosition b) [(p1, p2) | p2 <- [1..8]] | p1 <- [1..8]]

generateBoardMovesForColour :: Board -> Colour -> [[[BoardPosition]]]
generateBoardMovesForColour b c = [map (\x -> validatePositionWithColour b x c) [(p1, p2) | p2 <- [1..8]] | p1 <- [1..8]]

validatePosition :: Board -> BoardPosition -> [BoardPosition]
validatePosition b p
                | (isPositionEmpty p b) = []
                | otherwise = filter (\x -> not (isUnderCheck p x b)) (generateLegalMoves p b)

validatePositionWithColour :: Board -> BoardPosition -> Colour -> [BoardPosition]
validatePositionWithColour b p c
                | (isPositionEmpty p b) = []
                | otherwise = filter (\x -> not (isUnderCheck p x b) && (isOccupiedByColour c p b)) (generateLegalMoves p b)

generateLegalMoves :: BoardPosition -> Board -> [BoardPosition]
generateLegalMoves p b = case (getPieceInPosition p b) of
                             Nothing -> []
                             Just (Piece _ pi) -> getMovementsForPiece pi p b

generateMoves :: BoardPosition -> Board -> [Board]
generateMoves (p1, p2) b = map (\x -> movePieceBetweenPositions (p1, p2) x b) ((generateBoardMoves b) !! (p1-1) !! (p2-1))

positionsWithColour :: Colour -> Board -> [BoardPosition]
positionsWithColour c b = 
                filter (\x -> isOccupiedByColour c x b) listOfPositions
                where listOfPositions = filter (\x -> not (isPositionEmpty x b)) [(i,j) | i <- [1..8], j<- [1..8]]

-- Without castling and enpassant
generateNextStates :: GameState -> [GameState]
generateNextStates (colour, board) = [(opponent colour, board2) | positions <- positionsWithColour colour board, board2 <- generateMoves positions board]

-- With all states (no stalemate)
generateAllNextStates :: Game -> [GameState]
generateAllNextStates (_, currentState, []) = generateNextStates currentState
generateAllNextStates (_, currentState, history)
                | null enPassantPossibility = generateNextStates currentState ++ [(opponent currentTurn, board2) | board2 <- castling kingMoved kingSideRookMoved queenSideRookMoved currentTurn board]
                | otherwise      = generateNextStates currentState ++ [(opponent currentTurn, board2) | board2 <- castling kingMoved kingSideRookMoved queenSideRookMoved currentTurn board] ++ [(opponent currentTurn, board2) | board2 <- enPassantMove currentState (head enPassantPossibility)]
                where currentTurn = fst currentState
                      board = snd currentState
                      kingMoved = hasKingMovedAlready currentTurn history
                      kingSideRookMoved = hasKingSideRookMovedAlready currentTurn history
                      queenSideRookMoved = hasQueenSideRookMovedAlready currentTurn history
                      enPassantPossibility = isEnPassantMovePossible currentState (last history)

-- Given a piece and a move, get its original position to check validity
getCurrentPositionBasedOnMove :: Colour -> (PieceType, (Int,Int), Maybe Int) -> Board -> Maybe BoardPosition
getCurrentPositionBasedOnMove c (p, (x, y), col) b = if null anyMoves
                                                     then Nothing
                                                     else if (length anyMoves) > 1
                                                          then case col of
                                                                Nothing -> Just (head anyMoves)
                                                                Just col' -> if (snd $ head anyMoves) == col'
                                                                             then Just (head anyMoves)
                                                                             else if (snd $ head (tail anyMoves)) == col'
                                                                                  then Just (head (tail anyMoves))
                                                                                  else Nothing
                                                          else Just (head anyMoves)
                                                        where
                                                            anyMoves = filter (\a -> (x,y) `elem` (getMovementsForPiece p a b)) ownPiecePositions
                                                            ownPiecePositions = filter (\a -> not (isPositionEmpty a b) && isOccupiedByColour c a b && isPositionOccupiedByPiece p a b) [(u,v) | u <- [1..8], v <- [1..8]]

-- Produce a move for the AI
aiMakeMove :: Game -> GameState -> Game
aiMakeMove g newGameState = (aiColour, (newColour, newBoard), hist ++ [currentState])
                        where aiColour = getAIColour g
                              currentState = getGameState g
                              hist = getGameHistory g
                              colour = getCurrentColourFromGameState currentState
                              newBoard = getCurrentBoardFromGameState newGameState
                              newColour = opponent colour

-- Produce a move for the player
move :: Game -> BoardPosition -> BoardPosition -> Game
move g bp1 bp2 = (aiColour, (newColour, newBoard), hist ++ [currentState])
                 where aiColour = getAIColour g
                       currentState = getGameState g
                       hist = getGameHistory g
                       colour = getCurrentColourFromGameState currentState
                       board = getCurrentBoardFromGameState currentState
                       newBoard = movePieceBetweenPositions bp1 bp2 board
                       newColour = opponent colour

movePostCheck :: Game -> BoardPosition -> BoardPosition -> Game
movePostCheck g bp1 bp2 = (aiColour, (colour, newBoard), hist ++ [currentState])
                        where aiColour = getAIColour g
                              currentState = getGameState g
                              hist = getGameHistory g
                              colour = getCurrentColourFromGameState currentState
                              board = getCurrentBoardFromGameState currentState
                              newBoard = movePieceBetweenPositions bp1 bp2 board
                              newColour = opponent colour

-- Get a random move
getRandomNextState :: Game -> Int -> GameState
getRandomNextState gs t =
    let gen = mkStdGen t
        states = take 3 (generateAllNextStates gs)
        size = length states
        (index, newgen) = randomR (0, size-1) gen :: (Int, StdGen)
    in (states!!index)

-- Initial game state during start
initialGameState :: GameState
initialGameState = (White, initialBoard)

-- Sample boards and states (currently used for testing)
sampleBoard :: Board
sampleBoard = [[Just(Piece White Rook), Just(Piece White Knight), Just(Piece White Bishop), Just(Piece White Queen), Just(Piece White King), Just(Piece White Bishop), Just(Piece White Knight), Just(Piece White Rook)],
                [Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn)],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                [Just(Piece Black Knight), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                [Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn)],
                [Just(Piece Black Rook), Nothing, Just(Piece Black Bishop), Just(Piece Black Queen), Just(Piece Black King), Just(Piece Black Bishop), Just(Piece Black Knight), Just(Piece Black Rook)]]

sampleGameState :: GameState
sampleGameState = (White, sampleBoard)

sampleGameState2 :: GameState
sampleGameState2 = (White, sampleBoard2)

sampleBoard2 :: Board
sampleBoard2 = [[Just(Piece White Rook), Just(Piece White Knight), Just(Piece White Bishop), Just(Piece White Queen), Just(Piece White King), Just(Piece White Bishop), Just(Piece White Knight), Just(Piece White Rook)],
                [Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn), Just(Piece White Pawn)],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                [Just(Piece Black Knight), Just(Piece Black Pawn), Nothing, Nothing, Nothing, Nothing, Nothing , Nothing],
                [Just(Piece Black Pawn), Nothing, Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn), Just(Piece Black Pawn)],
                [Just(Piece Black Rook), Nothing, Just(Piece Black Bishop), Just(Piece Black Queen), Just(Piece Black King), Just(Piece Black Bishop), Just(Piece Black Knight), Just(Piece Black Rook)]]

-- Initialization of games
initializeWhiteAIGame :: Game
initializeWhiteAIGame = (White, initialGameState, [])

initializeBlackAIGame :: Game
initializeBlackAIGame = (Black, initialGameState, [])

initializeAILessGame :: Game
initializeAILessGame = (White, initialGameState, [])

rookCount :: Colour -> Board -> Int
rookCount c b = length $ filter (\x -> (isOccupiedByColour c x b) && (isPositionOccupiedByPiece Rook x b)) [(u,v) | u <- [1..8], v <- [1..8]]

bishopCount :: Colour -> Board -> Int
bishopCount c b = length $ filter (\x -> (isOccupiedByColour c x b) && (isPositionOccupiedByPiece Bishop x b)) [(u,v) | u <- [1..8], v <- [1..8]]
