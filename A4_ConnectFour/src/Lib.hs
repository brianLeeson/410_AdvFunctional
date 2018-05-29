module Lib where

import qualified Data.Map as Map

-- Pieces - either X or O
data Piece = X | O
  deriving (Show)
-- Columns
-- Board - list of columns, col major

-- consider representing the board grid as a map from x-y coordinates to the value of that grid location. Data.Map (from containers)



