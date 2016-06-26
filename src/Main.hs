-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- 
--
-----------------------------------------------------------------------------

module Main (
  main
) where

import Data.List
import System.IO
import GameTree

data Color = B | W deriving (Eq, Show)

type Row a = [a] 
type Col a = [a]

-- A board is defined as 7 columns with 6 rows, since moves are made
-- by giving the column. A field is either empty (Nothing) or occupied
-- by a white (Just W) or black (Just B) piece.
type Board = Col (Row (Maybe Color))

-- The transposed board is used to output the board
type BoardT = Row (Col (Maybe Color))  -- Columns (Rows)

main = do
       putStrLn "Welcome to the game four-in-a-row!"
       gameloop 0 startBoard

gameloop :: Int -> Board -> IO()
gameloop moveNr board 
         = do
              printEmptyLine
              printPosition moveNr board
              printEmptyLine
              if even moveNr 
                 then
                    putStr "White, please enter the column for your move (1-7): "
                 else
                    putStr "Black, please enter the column for your move (1-7): "              
              hFlush stdout
              input <- getLine
              if validInput board input
                 then
                   do 
                    let move = read input - 1 
                    let newBoard = (makeMove board 
                                                (colorToMove (moveNr + 1)) move)
                    let newMoveNr = moveNr + 1
                    printEmptyLine
                    if isWin newBoard move 
                       then
                          do
                            if even newMoveNr
                               then
                                  putStrLn "--- Black wins ---"
                               else
                                  putStrLn "--- White wins ---"
                            printEmptyLine
                            printPosition newMoveNr newBoard
                       else
                            if newMoveNr < 42 
                               then
                                  gameloop newMoveNr newBoard 
                               else
                                  do 
                                    putStrLn "--- Game Drawn ---"
                                    printPosition newMoveNr newBoard
                else  
                    do 
                       putStrLn "Invalid Move!" 
                       gameloop moveNr board

validInput :: Board -> String -> Bool
validInput board input = if length input == 1 then
                           if read input >= 1 && read input <= 7 then
                              if not $ columnFull (board !! (read input - 1)) then
                                 True
                              else
                                  False
                           else
                               False
                        else 
                           False

columnFull :: Col (Maybe Color) -> Bool
columnFull [] = True
columnFull (Nothing:ps) = False
columnFull (Just _:ps) = columnFull ps
                    
colorToMove :: Int -> Color
colorToMove move = if even move then
                      B
                   else 
                      W

-- 'printEmptyLine' prints an empty line
printEmptyLine = putStrLn " "

-- 'printPosition' prints the position
printPosition moveNr board 
   = do
       putStrLn $ "Position after move " ++ show moveNr
          ++ "  ==>  Eval: " ++ show (evaluate board)
       putStrLn " "
       putStr $ showBoard board


-- At the start of the game, the board is empty
startBoard :: Board
startBoard = replicate 7 $ replicate 6 Nothing

-- In order to show the board, the board is transposed
showBoard :: Board -> String
showBoard board = showBoardT $ transpose $ map reverse board
 
showBoardT :: BoardT -> String
showBoardT []     = showColumnNames
showBoardT (r:rs) = showRow r ++ showBoardT rs

showRow = foldr (++) ("|\n") . map showPiece

showPiece Nothing  = "|   "
showPiece (Just B) = "| B "
showPiece (Just W) = "| W "

showColumnNames =    "+---+---+---+---+---+---+---+\n"
                  ++ "  1   2   3   4   5   6   7  \n"

makeMove :: Board -> Color -> Int -> Board
makeMove board color column = changeElem column newCol board  
   where newCol = putPieceInColumn color (board !! column)

putPieceInColumn :: Color -> [Maybe Color] -> [Maybe Color]
putPieceInColumn piece [] = error "Column full"
putPieceInColumn piece (Nothing:cs)  = (Just piece):cs
putPieceInColumn piece (p:cs) = p : putPieceInColumn piece cs

-- Victory Conditions

checkCol :: Board -> Int -> Maybe Color
checkCol board col = checkLine W 0 (board !! col) 

checkRow :: Board -> Int -> Maybe Color
checkRow board col = checkLine W 0 ((transpose board) !! col) 

checkDiagonals :: Board -> Int -> Maybe Color
checkDiagonals = undefined

checkLine :: Color -> Int -> [Maybe Color] -> Maybe Color
checkLine _ _ []     = Nothing
checkLine W n (x:xs) = if x == Just W then
                          if n >= 3 then
                              Just W
                          else
                              checkLine W (n + 1) xs
                      else 
                          checkLine B 1 xs
checkLine B n (x:xs) = if x == Just B then
                          if n >= 3 then
                              Just B
                          else
                              checkLine B (n + 1) xs
                      else 
                          checkLine W 1 xs


-- 'fourInARow' returns True, if there are more than four equal adjacent pieces in a list. Nothing is not counted
fourInARow :: [Maybe Color] -> Bool
fourInARow []           = False
fourInARow (Nothing:xs) = fourInARow' W 0 xs
fourInARow (Just x:xs)  = fourInARow' x 1 xs 

fourInARow' x n []     = n >= 4
fourInARow' x n (Nothing:ys) = fourInARow' W 0 ys  
fourInARow' x n (Just y:ys) 
   | x == y = if n >= 3 then 
                  True
              else
                  fourInARow' x (n + 1) ys
   | otherwise = fourInARow' y 1 ys

-- 'isWin' checks, if a new move results in a winning position
isWin matrix col = or $ map fourInARow (candidateLines matrix row col)
    where row = fst $ squareLastMove matrix col

squareLastMove matrix col 
   = (row, col)
     where row = currentRow (matrix !! col)

currentRow [] = -1
currentRow (Nothing:xs) = -1
currentRow (Just _:xs) = 1 + currentRow xs

-- 'candidateLines' returns the row, column and the two diagonals that need to be checked for four pieces in a row
candidateLines matrix row col 
    =    colMatrix matrix col
      :  rowMatrix matrix row
      :  diagonalUp matrix row col
      : [diagonalDown matrix col row] 

colMatrix matrix col = matrix !! col

rowMatrix matrix row = (transpose matrix) !! row

diagonalUp matrix row col 
   | row <= col = diagonalUp' matrix 0 (col - row)
   | otherwise  = diagonalUp' matrix (row - col) 0

diagonalUp' matrix row col
   | row <= 5 && col <= 6 
        = square : diagonalUp' matrix (row + 1) (col + 1)
   | otherwise 
        = []
     where square = squareMatrix matrix row col  

diagonalDown matrix row col 
   | row + col <= 5 = diagonalDown' matrix (row + col) 0
   | otherwise  = diagonalDown' matrix 5 (row + col - 5)

diagonalDown' matrix row col
   | row >= 0 && col <= 6 
        = square : diagonalDown' matrix (row - 1) (col + 1)
   | otherwise 
        = []
     where square = squareMatrix matrix row col 

squareMatrix matrix row col = (matrix !! col) !! row

-- Help Functions
changeElem _ _ [] = []
changeElem 0 x (y:ys) = x : ys
changeElem n x ys 
    | n < length ys = take n ys ++ [x] ++ drop (n+1) ys
    | otherwise = ys

data Position = WinWhite Board
              | WinBlack Board
              | Draw Board
              | NoDecision Board
                deriving (Eq, Show) 

gameTree color board = createTree [0..6] color board

-- Problem: Victory positions are not detected!
--
-- Better: gameTree moveNr position = createTree [0..6] moveNr position
--    where position can be Win or Draw! Color can then be derived using 
--    the moveNumber
-- The GameTree should then have the type Node (Color, Move, Position)

createTree moves color board = Node board (mapFilter (createTree moves (switchColor color) . (makeMove board color)) (validMove board) moves)

infEvalTree :: Color -> Board -> GameTree (Int, Board)
infEvalTree color board = infEvalTree' color (-1, board) [0..6]

infEvalTree' :: Color -> (Int, Board) -> [Int] -> GameTree (Int, Board)
infEvalTree' color (move, board) moves
  = Node (move, board) (newPositions board validMoves)
  where validMoves = filter (validMove board) moves
        newPositions board []     = []
--        newPositions board (m:ms) = [Node (move, board) []]
        newPositions board (m:ms)
          = infEvalTree' newColor (m, makeMove board color m) moves : newPositions board ms 
        newColor = switchColor color


  
isWhiteWin = False
isBlackWin = False
isDraw = False

pruneTree 0 (Node x tree) = Node x []
pruneTree n (Node x tree) = Node x (map (pruneTree (n-1)) tree) 

mapTree f (Node x [])   = Node (f x) []
mapTree f (Node x tree) = Node (f x) (map (mapTree f) tree)

switchColor W = B
switchColor B = W

mapFilter f p [] = []
mapFilter f p (x:xs) = if p x then
                          f x : mapFilter f p xs
                       else
                          mapFilter f p xs

validMove board col = not $ columnFull (board !! col)



-- Evaluation: Counts number of possible wins in a given position

evaluate matrix = evaluatePosition W matrix - evaluatePosition B matrix

evaluatePosition :: Color -> Board -> Int
evaluatePosition color matrix
  = sum $ map sum [ potWinsField matrix color row col | row <- [0..6], col <- [0..5]]

potWinsField matrix color row col
  =   potWinsLine color col (colMatrix matrix col)
    : potWinsLine color row (rowMatrix matrix col)
    : potWinsLine color row (diagonalUp matrix row col)
    : potWinsLine color col (diagonalDown matrix row col)
    : []
      
potWinsLine :: Color -> Int -> [Maybe Color] -> Int
potWinsLine c n xs = sum $ map trueToOne $ map (and . map (== (Just c))) $ cand $ cutList n $ map (setColor c) xs
  where trueToOne True  = 1
        trueToOne False = 0


cutList n xs = if n > 3 then
                 drop (n-3) xs
               else
                 if length xs > n + 4 then
                   take (n+4) xs
                 else
                   xs

cand xs = filter ((>= 4) . length) $ map (take 4) $ tails xs

setColor :: Color -> Maybe Color -> Maybe Color
setColor x (Just c) = Just c
setColor x Nothing  = Just x
 
mapBoard f = map (map f)

createEvaluationTree :: Int -> Board -> Color -> GameTree Board
createEvaluationTree halvmoves currentPosition colorToMove
   = prune halvmoves $ createTree [0..6] colorToMove currentPosition 


evaluateMove :: Color -> GameTree Board -> Int
evaluateMove color board = evaluateMove' color (mapTree evaluate board)

evaluateMove' :: Color -> GameTree Int -> Int
evaluateMove' _     (Node eval []) = eval
evaluateMove' W     (Node eval ns) = maximum' (-1000) $ map (evaluateMove' W) ns
evaluateMove' B     (Node eval ns) = minimum' 1000 $ map (evaluateMove' B)  ns



-- TEST

emptyCol :: [Maybe Color]
emptyCol = replicate 6 Nothing

emptyRow :: [Maybe Color]
emptyRow = replicate 7 Nothing

matrix = map matrixCol col
   where
     matrixCol x = [(a, x) | a <- row]
     col = [0..6]
     row = [0..5]

candidates matrix col = candidateLines matrix row col
    where row = fst $ squareLastMove matrix col

piecesInColumn [] = 0
piecesInColumn (Nothing:cs) = 0
piecesInColumn (p:cs) = 1 + piecesInColumn cs

evalBoard = map piecesInColumn

testboard = testboard' 0

testboard' 7 = []
testboard' n = take 6  (iterate (+1) (n*10)) : testboard' (n+1)






