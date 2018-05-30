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

-- function takes a tuple (a,b), a Board and returns the Piece at Board[a][b], [0][0] is top left
-- TODO add error checking?
boardPeek :: (Int, Int) -> Board -> Maybe Piece
boardPeek (row, col) board = do 
    colMap <- Map.lookup col board
    Map.lookup row colMap

-- function takes a tuple (a,b), a Board, a Piece, and returns an updated Board with the piece at Board[a][b]
boardInsert :: (Int, Int) -> Piece -> Board -> Maybe Board
boardInsert (row, col) piece board = do 
    oldCol <- Map.lookup col board    
    Just (Map.insert col (Map.insert row piece oldCol) board)

-- function that takes a column and a piece and places that piece in that column. 
-- evaluates to nothing if that column doesn't exist or is full
-- evaluates to Just  an updated board otherwise





