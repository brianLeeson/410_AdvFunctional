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
  | x > y = balance (N color left y (ins x right))
  | x < y = balance (N color (ins x left) y right)

insert :: Ord a => a -> RBTree a -> RBTree a
insert x t = blackenRoot (ins x t)

-- Exercise 2.1
toList :: RBTree a -> [a]
toList L = []
toList (N color left val right) = (toList left) ++ [val] ++ (toList right)

-- fromList :: Ord a => [a] -> RBTree a
fromList [] = L
fromList (item: itemList) = insert item (fromList itemList)

-- trees are equal if their in order traversal values are equal.
instance Eq a => Eq (RBTree a) where
  tree1 == tree2 = (toList tree1) == (toList tree2)

-- Exercise 2.2
rootColor :: RBTree a -> Color
rootColor L = B
rootColor (N color left val right) = color

--colorValue :: Color -> Int
colorValue R = 0
colorValue B = 1

-- Exercise 2.3
type Path a = [(Color, a)]


