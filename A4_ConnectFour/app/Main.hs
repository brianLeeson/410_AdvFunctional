-- Author: Brian Leeson

module Main where

import Lib
import qualified Data.Map as Map
import Data.Char
import System.Random

main :: IO ()
main = do
    putStrLn "--- Welcome ConnectFour!!!! ---"
    forever (newGame)
    
newGame :: IO ()
newGame = do
    putStrLn "\nStarting a new game..."
    numPlayers <- prompt "How many human players? (1 or 2)"
    if numPlayers == "1" then putStrLn ("Player1 vs. Computer!\n") else putStrLn ("Player1 vs. Player2!\n")
    playGame numPlayers X emptyBoard
    return ()

-- Have the user pick a column to play in. Returns True if four of the same pieces are connected, meaning game over
playGame :: String -> Piece -> Board -> IO ()
playGame numPlayers currentPiece currentBoard = do
    --display
    putStrLn "  0   1   2   3   4   5   6  "
    displayBoard 0 0 currentBoard
    

    if (numPlayers == "1") && (currentPiece == O) 
        then do
                   roboChoice <- randomRIO (colMin, colMax)
                   putStrLn ("BEEP BOOP, PICKED COLUMN " ++ (show roboChoice))
                   case playPiece rowMax roboChoice currentPiece currentBoard  of
                        -- placing failed. column full. try again
                        Nothing -> do putStrLn ("Column " ++  (show roboChoice) ++ " is full, please try again")
                                      playGame numPlayers currentPiece currentBoard
                        -- placing succeded. continue
                        Just newBoard -> do let continue = isGameOver newBoard in 
                                                 case continue of
                                                     False -> do 
                                                                case currentPiece of
                                                                    X -> playGame numPlayers O newBoard
                                                                    O -> playGame numPlayers X newBoard
                                                     True -> do putStrLn "  0   1   2   3   4   5   6  "
                                                                displayBoard 0 0 newBoard
                                                                putStrLn (" --- Game Over --- \n ---- " ++ (show currentPiece) ++ " wins! ---- ")
        else do

            --ask for player move
            putStrLn ("Current player is: " ++ (show currentPiece))
            col <- prompt "Which column would you like to play in? [0-6]"
            let pickedColumn = read col :: Int

            -- check if valid move
            let isValidCol = ((0 <= pickedColumn) && (pickedColumn <= 6))
            case isValidCol of 
                -- chosen col num is out of bounds
                False -> do putStrLn ("invalid column " ++ col ++ ", please try again")
                            playGame numPlayers currentPiece currentBoard
                -- col inbounds. attempt to place piece in column at bottom of column
                True -> do let attemptToPlace = playPiece rowMax pickedColumn currentPiece currentBoard in -- initially attempt to place at bottom
                               case attemptToPlace of
                                   -- placing failed. column full. try again
                                   Nothing -> do putStrLn ("Column " ++ col ++ " is full, please try again")
                                                 playGame numPlayers currentPiece currentBoard
                                   -- placing succeded. continue
                                   Just newBoard -> do let continue = isGameOver newBoard in 
                                                           case continue of
                                                               False -> do 
                                                                          case currentPiece of
                                                                              X -> playGame numPlayers O newBoard
                                                                              O -> playGame numPlayers X newBoard
                                                               True -> do putStrLn "  0   1   2   3   4   5   6  "
                                                                          displayBoard 0 0 newBoard
                                                                          putStrLn (" --- Game Over --- \n ---- " ++ (show currentPiece) ++ " wins! ---- ")



  

    

