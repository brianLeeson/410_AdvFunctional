module Main where

import Lib
import qualified Data.Map as Map
import Data.Char

main :: IO ()
main = do
    putStrLn "--- Welcome ConnectFour ---!"
    forever (newGame)
    
newGame :: IO ()
newGame = do
  --secret <- randomRIO range
    --let (low, high) = range
    putStrLn "\nStarting a new game..."
    numPlayers <- prompt "How many human players? (1 or 2)"
    if numPlayers == "1" then putStrLn ("Player1 vs. Compueter!") else putStrLn ("Player1 vs. Player2!")
    repeatUntilTrue (playGame numPlayers emptyBoard)
    return ()

-- Have the user pick a column to play in. Returns True if four of the same pieces are connected, meaning game over
playGame :: String -> Board -> IO Bool
playGame numPlayers currentBoard = do
    
    -- make move

    -- check game state

    -- return Bool based on game state


    answer <- prompt "What's my number?"
    let guess = read answer :: Int
        sensible = all isDigit answer
    case (sensible, compare guess 4) of
        (False, _) -> do putStrLn "That's not even a number!"
                         return False
        (True, LT) -> do putStrLn "That's too low."
                         return False
        (True, GT) -> do putStrLn "That's too high."
                         return False
        (True, EQ) -> do putStrLn "That's it!"
                         return True

    

