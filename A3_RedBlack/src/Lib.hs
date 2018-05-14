module Lib where

import Test.QuickCheck hiding (shuffle)
import Test.Hspec

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

fromList :: Ord a => [a] -> RBTree a
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

paths :: RBTree a -> [Path a]
paths L = [[]]
paths (N color left value right) = [(color, value) : p | p <- paths left ] ++  [(color, value) : p | p <- paths right ]

-- Exercise 3.1
instance (Arbitrary a, Ord a) => Arbitrary (RBTree a) where
  arbitrary = do
    a <- arbitrary
    return (fromList a)

-- Exercise 3.2
-- found on Stack
isSorted :: (Ord a) => [a] -> Bool
isSorted []       = True
isSorted [x]      = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

inOrder :: RBTree Int -> Bool
inOrder L = True
inOrder tree = isSorted treeList
  where treeList = toList tree


isBlackRoot :: RBTree Int -> Bool
isBlackRoot L = True
isBlackRoot (N color left val right) = color == B

childColor L = B
childColor (N color left value right) = color

noRedChain :: RBTree Int -> Bool
noRedChain L = True
noRedChain (N B left value right) = (noRedChain left) && (noRedChain right)
noRedChain (N R left value right) = 
  if (childColor left) /= R && (childColor right) /= R 
  then (noRedChain left) && (noRedChain right) 
  else False

valuatePath [] = 0
valuatePath (x:xs) = case x of
  (B, _) -> 1 + (valuatePath xs)
  (R, _) -> 0 + (valuatePath xs)

-- allTheSame found on stack
allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)

samePathValues :: RBTree Int -> Bool
samePathValues L = True
samePathValues tree = allTheSame [(valuatePath path) | path <- (paths tree)]








