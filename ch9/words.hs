module Words where

myWords :: String -> [String]
myWords "" = []
myWords str =
  [takeWhile (/=' ') str] ++
  myWords (dropWhile (==' ') (dropWhile (/=' ') str)) 
