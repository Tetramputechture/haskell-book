module MyZip where

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip xs ys = [(head xs, head ys)] ++ myZip (tail xs) (tail ys)

myZipWith :: (a -> b -> c) 
          -> [a] -> [b] -> [c]
myZipWith fn [] _ = []
myZipWith fn _ [] = []
myZipWith fn xs ys = [fn (head xs) (head ys)] ++ myZipWith fn (tail xs) (tail ys)
