module Validate where

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

isVowel :: Char -> Bool
isVowel c = elem c vowels

countVowels :: String -> Int
countVowels = length . filter isVowel

countConsonants :: String -> Int
countConsonants = length . filter (not . isVowel)

mkWord :: String -> Maybe Word'
mkWord str 
  | vCount < cCount = Just (Word' str)
  | otherwise = Nothing
  where vCount = countVowels str
        cCount = countConsonants str
