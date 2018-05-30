module Main where

import Lib
import qualified Data.Map as Map 

main :: IO ()
main = do
    let x = Map.insert 5 'x' (Map.fromList [(5,'a'), (3,'b')])
    putStrLn "Hello, world!"
    

    

