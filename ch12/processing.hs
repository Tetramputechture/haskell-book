module Processing where

notThe :: String -> Maybe String
notThe str
  | str /= "the" = Just str
  | otherwise = Nothing

translateWord :: Maybe String -> String
translateWord str = case str of
  Just str -> str
  Nothing -> "a"

replaceThe :: String -> String
replaceThe = unwords . map translateWord . map notThe . words

isVowel :: Char -> Bool
isVowel x = elem x "aeiou"

startsWithVowel :: String -> Bool
startsWithVowel = isVowel . head

countTheBeforeVowel :: String -> Int
countTheBeforeVowel str = countTheBeforeVowel' $ words str

countTheBeforeVowel' :: [String] -> Int
countTheBeforeVowel' [] = 0
countTheBeforeVowel' (x:xs)
  | xs == [] = 0
  | x == "the" && startsWithVowel (head xs) = 1 + countTheBeforeVowel' xs
  | otherwise = countTheBeforeVowel' xs

countVowels :: String -> Int
countVowels = length . filter isVowel

  

