module Main where

import Lib
import System.Random
import Data.Char
import A1
import System.Exit

main :: IO ()
main = do
  putStrLn "Hello, World!"  

--Part 1
freshDeck :: IO Deck
freshDeck = do
  generator <- newStdGen
  let infList = randoms generator
  return (shuffle infList fullDeck)
  
draw :: Deck -> IO (Card, Deck)
draw deck = do
  case deck of
    [] -> do
      putStrLn "shuffling a new deck."
      (nCard : nDeck) <- freshDeck
      return (nCard, nDeck)
    (card : remainingDeck) -> do
      return (card, remainingDeck)

hitHand :: Hand -> Deck -> IO (Hand, Deck)
hitHand hand deck = do
  (card, remainingDeck) <- (draw deck)
  return (hand ++ [card], remainingDeck)

deal :: Deck -> IO (Hand, Deck)
deal deck = do
  (hand, remaingDeck) <- (hitHand [] deck)
  hitHand hand remaingDeck

prettyPrint :: Hand -> String
prettyPrint hand =
  show hand ++ " (" ++ show (handValue hand) ++ ")"

prompt2 :: String -> String -> (String -> Maybe a) -> (a -> IO b) -> IO b
prompt2 query help parse act = do
  putStrLn query
  input <- getLine
  case input of
    "quit" -> do 
      exitSuccess
    "help" -> do
      putStrLn help
      prompt2 query help parse act
    _ -> do 
      case parse input of
        Nothing -> do
          putStrLn "I didn't understand that."
          putStrLn help
          prompt2 query help parse act
        Just x -> do act x

data Move = Hit | Stand

parseMove :: String -> Maybe Move
parseMove "hit" = Just Hit
parseMove "stand" = Just Stand
parseMove _ = Nothing

dealerTurn :: Hand -> Deck -> IO (Hand, Deck)
dealerTurn hand deck = do
  case compare (handValue hand) 16 of
    LT -> do
      (hand, deck) <- hitHand hand deck
      dealerTurn hand deck
    _ -> return (hand, deck)


--playerTurn :: Hand -> Deck -> IO (Hand, Deck)
playerTurn hand deck = do
  prompt2 ("Your hand is " ++ prettyPrint hand ++ ", what do you do?") "You can either hit or stand" parseMove playerAction
  where playerAction move = case move of
          Hit -> do
            (hand, deck) <- hitHand hand deck
            case compare (handValue hand) 21 of
              GT -> do 
                return (hand, deck)
              _ -> prompt2 ("Your hand is " ++ prettyPrint hand ++ ", what do you do?") "You can either hit or stand" parseMove playerAction
          Stand -> return (hand, deck)



    

