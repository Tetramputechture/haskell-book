module Std where

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny fn xs = fn (head xs) || myAny fn (tail xs)

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a xs = any (==a) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = [last xs] ++ myReverse (init xs)

mySquish :: [[a]] -> [a]
mySquish [] = []
mySquish xs = head xs ++ mySquish (tail xs)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap fn xs = fn (head xs) ++ squishMap fn (tail xs)

squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain (x:xs) = x ++ (squishMap id xs)

myMaximumBy :: (a -> a -> Ordering)
            -> [a] -> a
myMaximumBy _ [a] = a
myMaximumBy fn (x:y:xs)  
  | fn x y == GT = myMaximumBy fn (x:xs)
  | otherwise = myMaximumBy fn (y:xs)

myMinimumBy :: (a -> a -> Ordering)
            -> [a] -> a
myMinimumBy _ [a] = a
myMinimumBy fn (x:y:xs)
  | fn x y == LT = myMinimumBy fn (x:xs)
  | otherwise = myMinimumBy fn (y:xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
 
 
