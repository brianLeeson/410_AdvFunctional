module Main where

import Test.QuickCheck
import Test.Hspec

import Lib

main :: IO ()
main = do
  quickCheck inOrder          
  quickCheck isBlackRoot          
  quickCheck noRedChain         
  quickCheck samePathValues

    

