module Main where

import Lib
import qualified Data.Map as Map
import Data.Char

main :: IO ()
main = do
    putStrLn "Welcome ConnectFour!"
    forever (playGame)
    
playGame :: IO ()
playGame = do

  --secret <- randomRIO range
  --let (low, high) = range
  numPlayers <- prompt "How many human players? (1 or 2)"
  if numPlayers == "1" then putStrLn ("Player1 vs. Compueter!") else putStrLn ("Player1 vs. Player2! ")
  repeatUntilTrue (playround 5)
  return ()

-- Have the user make a guess at the secret number. Returns True if the guess
-- was correct, False otherwise.
playround :: Int -> IO Bool
playround secret = do
  answer <- prompt "What's my number?"
  let guess = read answer :: Int
      sensible = all isDigit answer
  case (sensible, compare guess secret) of
    (False, _) -> do putStrLn "That's not even a number!"
                     return False
    (True, LT) -> do putStrLn "That's too low."
                     return False
    (True, GT) -> do putStrLn "That's too high."
                     return False
    (True, EQ) -> do putStrLn "That's it!"
                     return True

    

