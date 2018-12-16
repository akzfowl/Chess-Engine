module Minmax where

import Board
import Piece
import Colour
import PieceType

data GameTree = GameTree {state::GameState, children::[GameTree]}

maxDepth :: Int
maxDepth = 2

generateTree :: Int -> Game -> GameTree
generateTree 0 (_, gs, _)         = GameTree gs []
generateTree curDepth (c, gs, gh)
        | endState gs = GameTree gs []
        | otherwise   = GameTree gs (map (generateTree (curDepth-1)) nextStates)
        where nextStates = map (\y -> (c, y, gs:gh)) (generateAllNextStates (c, gs, gh))

minmaxAlg :: GameTree -> Int
minmaxAlg (GameTree t [])          = evaluateState t
minmaxAlg (GameTree (White, _) xs) = maximum (map minmaxAlg xs)
minmaxAlg (GameTree (Black, _) xs) = minimum (map minmaxAlg xs)

retrieveNextState :: Game -> GameState
retrieveNextState g = case (generateTree maxDepth g) of
                          GameTree gs []     -> gs
                          GameTree (c, _) xs -> snd (findBestState c (compare c) (map (\x -> (minmaxAlg x, state x)) xs))
        where compare White = (>)
              compare Black = (<)                          

findBestState :: Colour -> (Int -> Int -> Bool) -> [(Int, GameState)] -> (Int, GameState)
findBestState _ _ [x] = x
findBestState c cmp ((x1,y1):xs)
        | winningState c y1 = (x1, y1)
        | otherwise = let (x2, y2) = findBestState c cmp xs in
                  if cmp x1 x2 then (x1, y1) else (x2, y2)
                  
endState :: GameState -> Bool
endState gs = ev > threshold || ev < - threshold
              where ev = evaluateState gs

winningState :: Colour -> GameState -> Bool
winningState White gs = evaluateState gs > threshold
winningState Black gs = evaluateState gs < (-threshold)

evaluateState :: GameState -> Int
evaluateState gs = totalEval (snd gs)

evaluateBoard :: Board -> Int
evaluateBoard b = let (pointsWhite, pointsBlack) = boardPieceEvaluator b in (pointsWhite - pointsBlack)

boardPieceEvaluator :: Board -> (Int,Int)
boardPieceEvaluator b = foldl addPieceValue (0,0) (concat b)
                        where addPieceValue pts Nothing = pts
                              addPieceValue (pointsWhite, pointsBlack) (Just (Piece c pt)) | c == White = (pointsWhite + pointValue pt, pointsBlack)
                                                                                           | otherwise = (pointsWhite, pointsBlack + pointValue pt)

centralSquares :: [BoardPosition]
centralSquares = [(4,4), (4,5), (5,4), (5,5)]

centralControlEvaluator :: Board -> Int
centralControlEvaluator b = (p1-p2)
                            where (p1,p2) = foldl (\x acc -> if isOccupiedByColour White x b then (fst acc +1, snd acc) else if isOccupiedByColour Black x b then (fst acc, snd acc+1) else acc) (0,0) centralSquares

evaluationWeights :: [Int]
evaluationWeights = [10, 2]

weightApplication :: [Int] -> [Int] -> Int
weightApplication [] _ = 0
weightApplication _ [] = 0
weightApplication (x:xs) (y:ys) = (x*y) + (weightApplication xs ys)

applyWeights :: [Int] -> Int
applyWeights criteria = weightApplication criteria evaluationWeights

totalEval :: Board -> Int
totalEval b = applyWeights [(evaluateBoard b),(centralControlEvaluator b)]

threshold :: Int
threshold = 400