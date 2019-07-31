module BuildingFunctions1 where

funcA :: [Char] -> [Char]
funcA str = str ++ "!"

funcB :: [a] -> [a]
funcB str = [str !! 4]

funcC :: [a] -> [a]
funcC str = drop 9 str

