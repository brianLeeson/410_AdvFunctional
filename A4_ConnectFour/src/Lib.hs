module Lib where

import qualified Data.Map as Map 

-- Pieces - either X or O or Empty space
data Piece = X | O | Empty
    deriving (Eq)
  
instance Show Piece where
  show X = "X"
  show O = "O"
  show Empty = "."

-- Columns are a map of ints to pieces
type Column = Map.Map Int Piece

-- Board - 6 rows 7 cols --  a map of maps -- COLUMN MAJOR matrix
-- Data.Map (from containers)
-- https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html
type Board = Map.Map Int Column

--Globals
rowMin = 0 :: Int
rowMax = 5 :: Int
colMin = 0 :: Int
colMax = 6 :: Int

emptyColumn :: Column
emptyColumn = Map.fromList([(0,Empty),(1,Empty),(2,Empty),(3,Empty),(4,Empty),(5,Empty)])

emptyBoard :: Board
emptyBoard = Map.fromList([(0, emptyColumn),(1, emptyColumn),(2, emptyColumn),(3, emptyColumn),(4, emptyColumn),(5, emptyColumn),(6, emptyColumn)])

testColumn1 :: Column
testColumn1 = Map.fromList([(0,Empty),(1,Empty),(2,Empty),(3,Empty),(4,Empty),(5,Empty)])

testColumn2 :: Column
testColumn2 = Map.fromList([(0,X),(1,X),(2,X),(3,X),(4,X),(5,X)])

testColumn3 :: Column
testColumn3 = Map.fromList([(0,Empty),(1,Empty),(2,X),(3,O),(4,O),(5,X)])

testBoard :: Board
testBoard = Map.fromList([(0, testColumn1),(1, testColumn2),(2, testColumn2),(3, testColumn2),(4, testColumn2),(5, testColumn3),(6, testColumn1)])

-- End Globals

-- function that takes a board a prints it.
displayBoard :: Int -> Int -> Board-> IO()
displayBoard rowIndex colIndex board = do
    let isValidRow = ((0 <= rowIndex) && (rowIndex <= 5)) in
        case isValidRow of
            False -> putStrLn "\n"
            True -> do displayRow rowIndex colIndex "| " board
                       displayBoard (rowIndex+1) colIndex board
    

-- function that takes a column and prints it
displayRow :: Int -> Int -> String -> Board-> IO()
displayRow rowIndex colIndex rowAsString board = do
    let maybePiece = boardPeek rowIndex colIndex board in
        case maybePiece of
            Nothing -> putStrLn (rowAsString ++ "\n-----------------------------")
            Just piece -> displayRow rowIndex (colIndex+1) (rowAsString ++ (show piece) ++ " | " ) board
        

-- function takes a tuple (row, col), a Board and returns the Piece at Board[col][row], [0][0] is top left
boardPeek :: Int -> Int -> Board -> Maybe Piece
boardPeek row col board = do 
    colMap <- Map.lookup col board
    Map.lookup row colMap

-- function takes a tuple (row,col), a Board, a Piece, and returns an updated Board with the piece at Board[col][row]. 
-- fails if board[col][row] doesn't exist or is already occupied
boardInsert :: (Int, Int) -> Piece -> Board -> Maybe Board
boardInsert (rowIndex, colIndex) piece board = do 
    oldCol <- Map.lookup colIndex board
    targetPiece <- Map.lookup rowIndex oldCol
    if targetPiece /= Empty 
        then Nothing -- space is taken by a piece already
        else Just (Map.insert colIndex (Map.insert rowIndex piece oldCol) board) -- space is empty

-- function that takes a column index, piece, board and places that piece in that column, in the lowest empty row
-- evaluates to nothing if that column doesn't exist or is full
-- evaluates to Just  an updated board otherwise
playPiece :: Int -> Int -> Piece -> Board -> Maybe Board
playPiece rowIndex colIndex piece board = do
    let isValidRow = ((0 <= rowIndex) && (rowIndex <= 5))
    case isValidRow of
        False -> Nothing
        True -> let attempt = boardInsert (rowIndex, colIndex) piece board in
                    case attempt of
                        Nothing -> playPiece (rowIndex-1) colIndex piece board
                        Just aBoard -> Just aBoard

isGameOver :: Board -> Bool
isGameOver board = 
    let
        row = checkRows 0 board
        col = checkCols 0 board
        bSlash1 = checkBckSlashs 0 0 board
        bSlash2 = checkBckSlashs 2 0 board
        fwdSlash1 = checkFwdSlashs 0 0 board
        fwdSlash2 = checkFwdSlashs 2 0 board
    in row || col || bSlash1 || bSlash2 || fwdSlash1 || fwdSlash2


-- meant to start looking at (0,0)
checkRows :: Int -> Board -> Bool
checkRows rowIndex board =
    -- find initial piece
    let maybePiece = boardPeek rowIndex colMin board in
        case maybePiece of
            --This should happen when we run out of rows
            Nothing -> False 
            -- start looking at initalPosition+1
            Just initialPiece -> let itsOver = checkRow rowIndex (colMin+1) initialPiece 1 board in
                                     if itsOver then True else checkRows (rowIndex+1) board

checkRow :: Int -> Int -> Piece -> Int -> Board -> Bool
checkRow rowIndex colIndex prevPiece seqCount board =
    --Base Case: seqCount == 4 in a row
    if seqCount == 4 then True else

    -- find current piece (board[rowIndex][colIndex])
    let maybePiece = boardPeek rowIndex colIndex board in
        case maybePiece of
            Nothing -> False -- we've run off the board before finding a sequence
            Just curPiece -> do 
                                if (curPiece == prevPiece) && (curPiece /= Empty)
                                    -- found a run of at least two
                                    then checkRow rowIndex (colIndex+1) curPiece (seqCount+1) board
                                    -- run stopped. reset seqCount
                                    else checkRow rowIndex (colIndex+1) curPiece 0 board

-- meant to start looking at (0,0)
checkCols :: Int -> Board -> Bool
checkCols colIndex board =
    -- find initial piece
    let maybePiece = boardPeek rowMin colIndex board in
        case maybePiece of
            --This should happen when we run out of cols
            Nothing -> False 
            -- start looking at initalPosition+1
            Just initialPiece -> let itsOver = checkCol (rowMin+1) colIndex initialPiece 1 board in
                                     if itsOver then True else checkCols (colIndex+1) board

checkCol :: Int -> Int -> Piece -> Int -> Board -> Bool
checkCol rowIndex colIndex prevPiece seqCount board = 
    --Base Case: seqCount == 4 in a row
    if seqCount == 4 then True else

    -- find current piece (board[rowIndex][colIndex])
    let maybePiece = boardPeek rowIndex colIndex board in
        case maybePiece of
            Nothing -> False -- we've run off the board before finding a sequence
            Just curPiece -> do 
                                if (curPiece == prevPiece) && (curPiece /= Empty)
                                    -- found a run of at least two
                                    then checkCol (rowIndex+1) colIndex curPiece (seqCount+1) board
                                    -- run stopped. reset seqCount
                                    else checkCol (rowIndex+1) colIndex curPiece 0 board

-- meant to start looking at (0,0)
checkBckSlashs :: Int -> Int -> Board -> Bool
checkBckSlashs rowIndex colIndex board = 
    -- find initial piece
    let maybePiece = boardPeek rowIndex colIndex board in
        case maybePiece of
            --This should happen when we run off the board
            Nothing -> False 
            -- start looking at initalPosition+1
            Just initialPiece -> let itsOver = checkBckSlash (rowIndex+1) (colIndex+1) initialPiece 1 board in
                                     if itsOver then True else checkBckSlashs rowIndex (colIndex+1) board

checkBckSlash :: Int -> Int -> Piece -> Int -> Board -> Bool
checkBckSlash rowIndex colIndex prevPiece seqCount board = 
    --Base Case: seqCount == 4 in a row
    if seqCount == 4 then True else

    -- find current piece (board[rowIndex][colIndex])
    let maybePiece = boardPeek rowIndex colIndex board in
        case maybePiece of
            Nothing -> False -- we've run off the board before finding a sequence
            Just curPiece -> do 
                                if (curPiece == prevPiece) && (curPiece /= Empty)
                                    -- found a run of at least two
                                    then checkBckSlash (rowIndex+1) (colIndex+1) curPiece (seqCount+1) board
                                    -- run stopped. reset seqCount
                                    else checkBckSlash (rowIndex+1) (colIndex+1) curPiece 0 board

-- meant to start looking at (0,0)
checkFwdSlashs :: Int -> Int -> Board -> Bool
checkFwdSlashs rowIndex colIndex board = 
    -- find initial piece
    let maybePiece = boardPeek rowIndex colIndex board in
        case maybePiece of
            --This should happen when we run off the board
            Nothing -> False 
            -- start looking at initalPosition+1
            Just initialPiece -> let itsOver = checkFwdSlash (rowIndex+1) (colIndex-1) initialPiece 1 board in
                                     if itsOver then True else checkFwdSlashs rowIndex (colIndex+1) board

checkFwdSlash :: Int -> Int -> Piece -> Int -> Board -> Bool
checkFwdSlash rowIndex colIndex prevPiece seqCount board = 
    --Base Case: seqCount == 4 in a row
    if seqCount == 4 then True else

    -- find current piece (board[rowIndex][colIndex])
    let maybePiece = boardPeek rowIndex colIndex board in
        case maybePiece of
            Nothing -> False -- we've run off the board before finding a sequence
            Just curPiece -> do 
                                if (curPiece == prevPiece) && (curPiece /= Empty)
                                    -- found a run of at least two
                                    then checkFwdSlash (rowIndex+1) (colIndex-1) curPiece (seqCount+1) board
                                    -- run stopped. reset seqCount
                                    else checkFwdSlash (rowIndex+1) (colIndex-1) curPiece 0 board


-- Game Control, based on 410 lecture 3

-- Prompt a user for input by first displaying a question and then waiting for
-- their answer.
prompt :: String -> IO String
prompt question = do putStrLn question
                     getLine

-- For example, 'forever' runs the given IO action forever.
forever :: IO a -> IO b
forever action = do action
                    forever action

-- run the given action until it returns True.
repeatUntilTrue :: IO Bool -> IO ()
repeatUntilTrue action = do
  x <- action
  if x
    then return ()
    else repeatUntilTrue action


