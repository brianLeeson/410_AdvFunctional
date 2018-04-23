module A1 where

-- Author: Brian Leeson

import Data.List

-- Part 1
data Suit = Hearts | Diamonds | Clubs | Spades
    deriving (Eq, Enum)
data Royalty = Jack | Queen | King
    deriving (Eq, Enum, Ord)
data Ace = A
    deriving (Show, Eq)
data Rank = Ace | Royalty | Int --needed?? I think not
    deriving (Show, Eq)
data Card = Number Int Suit | Face Royalty Suit | High Suit

type Deck = [Card]
type Hand = [Card]

instance Show Suit where
  show Hearts = "\x2661"
  show Diamonds = "\x2662"
  show Clubs = "\x2663"
  show Spades = "\x2660"

instance Show Royalty where
  show Jack = "J"
  show Queen = "Q"
  show King = "K"

instance Show Card where
  show (Number i s) = show i ++ show s
  show (Face r s) =  show r ++ show s
  show (High s) = "A" ++ show s

suits::[Suit]
suits = [Hearts, Diamonds, Clubs, Spades]

faces::[Royalty]
faces = [Jack, Queen, King]

numbers::[Int]
numbers = [2, 3, 4, 5, 6, 7, 8, 9, 10]

combineFaces s [] = []
combineFaces s (x:xs) = [Face x s] ++ combineFaces s xs

combineNumbers s [] = []
combineNumbers s (x:xs) = [Number x s] ++ combineNumbers s xs

createDeck [] f n = []
createDeck (x:xs) f n = 
  combineNumbers x n ++ combineFaces x f ++ [High x] ++ createDeck xs f n

fullDeck = createDeck suits faces numbers

-- Part 2
cardValue c = case c of Number i _ -> i
                        Face _ _ -> 10
                        High _ -> 11

handValue :: Hand -> Int
handValue [] = 0
handValue (x:xs) = cardValue x + handValue xs

-- Part 4
data Indexed i a = Indx i a 
  deriving (Show)

instance Eq i => Eq (Indexed i a) where
  (Indx i a) == (Indx i' a') = i == i'

instance Ord i => Ord (Indexed i a) where
  compare (Indx x _) (Indx y _) = compare x y

createIndexList [] [] = []
createIndexList (a:as) (b:bs) = [Indx a b] ++ createIndexList as bs 

stripIndexList []  = []
stripIndexList ((Indx a b): as) = [b] ++ stripIndexList as

shuffle :: [Int] -> [a] -> [a]
shuffle a b = stripIndexList( sort (createIndexList a b ) )
