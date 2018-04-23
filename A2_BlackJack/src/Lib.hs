module Lib where

import Data.Char
import qualified Data.Map as M

-- Prompt a user for input by first displaying a question, waiting for their
-- answer, and then parsing their answer. If the user responds with something
-- unparsable (meaning that the given 'parse' function returns Nothing), then
-- prompt again, until the user responds with something that can be parsed
-- (meaning that the 'parse' function returns Just a result).
prompt :: String               -- Question to ask
       -> (String -> Maybe a)  -- Parse the answer
       -> IO a
prompt question parse = do
  putStrLn question
  answer <- getLine
  case parse answer of
    Just result -> return result
    Nothing     -> do
      putStrLn "I didn't understand that."
      prompt question parse

-- 'forever' runs the given IO action forever.
forever :: IO a -> IO b
forever action = do action
                    forever action

-- 'repeatUntilTrue' repeatedly runs the given action until it returns True.
repeatUntilTrue :: IO Bool -> IO ()
repeatUntilTrue action = do
  result <- action
  if result
    then return ()
    else repeatUntilTrue action


-- data Maybe a = Nothing | Just a
-- This data type is defined in the Prelude for you.

-- safeHead and safeTail are like the head and tail functions, but instead of
-- crashing they return Nothing when they are given the empty list.
safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead []     = Nothing

safeTail :: [b] -> Maybe [b]
safeTail (x:xs) = Just xs
safeTail []     = Nothing

-- safeDiv protects against a divide-by-zero error (which also crashes your
-- program) by returning Nothing when the divisor is 0.
safeDiv :: Integral a => a -> a -> Maybe a
x `safeDiv` 0 = Nothing
x `safeDiv` y = Just (x `div` y)

example1 = 5 `safeDiv` 2
example2 = 5 `safeDiv` 0

-- The read function is polymorphic, and what it does depends on its return
-- type, which is figured out by context. For example,
example3 = read "1234" + 1
-- because the result of read is added to 1, it must be some sort of numeric
-- type, which tells read how to parse the string "1234".

-- Unfortunately, the read function will crash if its given a string that it
-- doesn't know how to read. safeReadInt protects the read function for
-- producing Ints by sanity-checking the input string first. If the string is
-- non-empty, and contains only digit characters, then the read function for
-- Ints will safely return an Int value. Otherwise, read can crash, so return
-- Nothing.
safeReadInt :: String -> Maybe Int
safeReadInt s
  | isSafe s  = Just (read s)
  | otherwise = Nothing
  where isSafe s = all isDigit s && not (null s)

-- Key-value Maps, imported from Data.Map found in the containers package on
-- Hackage, are useful for representing things like phonebooks.
type Name = String
type PhoneNumber = String
type PhoneBook = M.Map Name PhoneNumber

-- Maps can be constructed using the fromList function.
book :: PhoneBook
book = M.fromList
       [("betty", "555-2938"),
        ("bonnie", "452-2928")]

-- Maps are persistant, and do not change over time. For example, the insert
-- function for maps returns a *new* map with the key-value binding inserted,
-- and the old map is still the same.
book' = M.insert "penny" "853-3592" book

-- The value associated with a key in a map can be found with the lookup
-- function, which returns Just the value if the key is found in the map, and
-- Nothing if the key is not found.

-- How can we lookup two PhoneNumbers in a PhoneBook, and return both if they
-- are both there?  This is not quite what we want. It returns the pair of
-- maybes, (Maybe PhoneNumber, Maybe PhoneNumber), instead of returning maybe a
-- pair, Maybe (PhoneNumber, PhoneNumber).
{-
lookupTwo n1 n2 bk = (M.lookup n1 bk, M.lookup n2 bk)
-}

-- This does what we want, but is tedious and repetitive to write and read.
{-
lookupTwo n1 n2 bk = case M.lookup n1 bk of
  Nothing -> Nothing
  Just p1 -> case M.lookup n2 bk of
    Nothing -> Nothing
    Just p2 -> Just (p1, p2)
-}
-- This does the same as the above correct function, but doesn't have the boring
-- cascade of cases.
lookupTwo :: Name -> Name -> PhoneBook -> Maybe (PhoneNumber, PhoneNumber)
lookupTwo n1 n2 bk = do
  p1 <- M.lookup n1 bk
  p2 <- M.lookup n2 bk
  Just (p1, p2)
-- The 'do' notation can be used for more than just IO! The meaning of the
-- binding statement depends on the return type of the 'do'. We saw before that
-- for an IO-style of do, a bind statement means to perform the input-output
-- action and bind the return result the variable. For this Maybe-style of do, a
-- bind statement means to bind the variable to the value inside of the Just on
-- the right of the arrow '<-'. If the value on the right of the arrow is a
-- Nothing, then the whole 'do' expression is Nothing.

-- For the Maybe a type,
--
--     return x :: Maybe a = Just x
