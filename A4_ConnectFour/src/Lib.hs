module Lib where

import qualified Data.Map as Map

-- Pieces - either X or O or Empty space
data Piece = X | O | Empty
  deriving (Show)

-- Columns
type Column = [Piece]

-- Board - 6 rows 7 cols
-- consider representing the board grid as a map from x-y coordinates to the value of that grid location. Data.Map (from containers)
-- https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html
type Board = [Column]

-- function takes a tuple (a,b), a Board and returns the value at Board[a][b]









