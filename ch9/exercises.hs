module Exercises where

import Data.Char

filterUpper :: String -> String
filterUpper str = filter isUpper str

capitalize :: String -> String
capitalize (x:xs) = [toUpper $ x] ++ xs

allCaps :: String -> String
allCaps "" = ""
allCaps (x:xs) = [toUpper x] ++ allCaps xs
