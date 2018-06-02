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


-- Game Control, based on lecture 3
theLine :: IO String
theLine = getLine

-- Prompt a user for input by first displaying a question and then waiting for
-- their answer.
prompt :: String -> IO String
prompt question = do putStrLn question
                     getLine

-- Greet the user by first asking for their name, and then saying hello.
greet :: IO ()
greet = do
  name <- prompt "Who is this?"
  let greeting = "Hello, " ++ name ++ "!"
  putStrLn greeting

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


