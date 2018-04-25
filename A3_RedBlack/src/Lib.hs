module Lib where

data Color = R | B
  deriving (Eq, Show)
data RBTree a = L | N Color (RBTree a) a (RBTree a)
  deriving (Show)


-- Exercise 1.1 
find :: Ord a => a -> RBTree a -> Maybe a
find target L = Nothing
find target (N _ left val right)
  | target == val = Just val
  | target < val = find target left
  | target > val = find target right
  | otherwise = Nothing 

-- Exercise 1.2
blackenRoot :: RBTree a -> RBTree a
blackenRoot L = L
blackenRoot (N _ left val right) = N B left val right

-- balance :: RBTree a -> RBTree a
balance tree = do 
  case tree of
    -- case 1
    (N B (N R (N R r x s) y t) z u) -> N R (N B r x s) y (N B t z u)
    -- case 2
    (N B r x (N R s y (N R t z u))) -> N R (N B r x s) y (N B t z u)
    -- case 3
    (N B r x (N R (N R s y t) z u)) -> N R (N B r x s) y (N B t z u)
    -- case 4
    (N B (N R r x (N R s y t)) z u) -> N R (N B r x s) y (N B t z u)
    _ -> tree

-- Exercise 1.3
ins :: Ord a => a -> RBTree a -> RBTree a
ins x L = (N R L x L)
ins x (N color left y right)
  | x == y = (N color left x right)
  | x > y = balance (ins x left)
  | x < y = balance (ins x right)


insert :: Ord a => a -> RBTree a -> RBTree a
insert x t = blackenRoot (ins x t)

-- Exercise 2.1
-- toList :: RBTree a -> [a]
-- fromList :: Ord a => [a] -> RBTree a




