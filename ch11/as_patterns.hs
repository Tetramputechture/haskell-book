module AsPatterns where

import Data.Char

isSubseqOf :: (Eq a)
           => [a]
           -> [a]
           -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf t@(x:xs) (y:ys) 
  | x == y = isSubseqOf xs ys
  | otherwise = isSubseqOf t ys

lastWords :: String -> String
lastWords = unwords . tail . words

firstWord :: String -> String
firstWord = head . words

capitalizeWord :: String -> String
capitalizeWord str = toUpper (head str) : (tail str)

capitalizeWords :: String
                -> [(String, String)]
capitalizeWords [] = []
capitalizeWords str@(x:xs) 
  | x == ' ' = capitalizeWords xs
  | otherwise = [(firstWord str, capitalizeWord (firstWord str))] ++ capitalizeWords (lastWords str)

capitalizeParagraph :: String -> String
capitalizeParagraph str@(x:xs)
  | xs == [] = []
  | x == '.' = capitalizeParagraph (tail xs)
  | otherwise = capitalizeWord (takeWhile (/= '.') str) ++ ". " ++ capitalizeParagraph (dropWhile (/= '.') str)
