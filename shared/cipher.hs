module Cipher where

import Data.Char

shiftRight :: Int -> Char -> Char
shiftRight _ ' ' = ' '
shiftRight 0 c = c
shiftRight 1 'z' = 'a'
shiftRight 1 'Z' = 'A'
shiftRight 1 c = chr $ ord c + 1 
shiftRight x c = chr $ (ord (shiftRight (x - 1) (shiftRight 1 c)))

rightCaesar :: Int -> String -> String
rightCaesar n str = map (shiftRight n) str  

shiftCountFromChar :: Char -> Int
shiftCountFromChar ' ' = 0
shiftCountFromChar c = ord (toLower c) - 97

vCipher :: String -> String -> String
vCipher keyword str = zipWith shiftRight (cycle (map shiftCountFromChar keyword)) str 

rightCaesarIO :: IO String
rightCaesarIO = do
  putStrLn "How many characters do you want to shift?"
  numString <- getLine
  let num = read numString
  putStrLn "What is your word"
  word <- getLine
  putStr "Your encrypted word is: "
  return $ rightCaesar num word

vCipherIO :: IO String
vCipherIO = do
  putStrLn "What is your keyword?"
  keyword <- getLine
  putStrLn "What is your word?"
  word <- getLine
  putStr "Your string is: "
  return $ vCipher keyword word
