module Palindrome where

import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (isAlpha, toLower)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome!"
    False -> exitSuccess

isPalindrome :: String -> Bool
isPalindrome str = modified == reverse modified
  where modified = [ toLower x | x <- str, isAlpha x ]
