module BreakString where

breakString :: String -> Char -> [String]
breakString "" _ = []
breakString str sep =
  [takeWhile (/= sep) str] ++
  breakString (dropWhile (== sep) (dropWhile (/= sep) str)) sep
