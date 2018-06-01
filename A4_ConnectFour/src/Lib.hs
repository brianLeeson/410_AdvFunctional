module Lib where

import qualified Data.Map as Map 

-- Pieces - either X or O or Empty space
data Piece = X | O | Empty
  
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

emptyColumn = Map.fromList([(0,Empty),(1,Empty),(2,Empty),(3,Empty),(4,Empty),(5,Empty),(6,Empty)])
emptyBoard = Map.fromList([(0, emptyColumn),(1, emptyColumn),(2, emptyColumn),(3, emptyColumn),(4, emptyColumn),(5, emptyColumn),(6, emptyColumn)])

-- function that takes a board a prints it.
displayBoard :: Board -> IO()
displayBoard = undefined

-- function that takes a column and prints it
displayColumn :: Board -> IO()
displayColumn = undefined

-- function takes a tuple (a,b), a Board and returns the Piece at Board[a][b], [0][0] is top left
boardPeek :: (Int, Int) -> Board -> Maybe Piece
boardPeek (row, col) board = do 
    colMap <- Map.lookup col board
    Map.lookup row colMap

-- function takes a tuple (a,b), a Board, a Piece, and returns an updated Board with the piece at Board[a][b]
boardInsert :: (Int, Int) -> Piece -> Board -> Maybe Board
boardInsert (row, col) piece board = do 
    oldCol <- Map.lookup col board    
    Just (Map.insert col (Map.insert row piece oldCol) board)

-- function that takes a column index, piece, board and places that piece in that column. 
-- evaluates to nothing if that column doesn't exist or is full
-- evaluates to Just  an updated board otherwise
playPiece :: Int -> Piece -> Board -> Board
playPiece col piece board = undefined



-- Game Control, based on lecture 3
theLine :: IO String
theLine = getLine

-- To chain together several actions, use a 'do' expression.
getTwoLines :: IO (String, String)
getTwoLines = do first <- getLine
                 second <- getLine
                 return (first, second)

putTwoLines :: String -> String -> IO ()
putTwoLines first second = do
  putStrLn first
  putStrLn second

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


