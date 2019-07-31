module FunctionsWithFolds where

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr 
       (\a b ->
         if a == True
         then True
         else b) False

myOr' :: [Bool] -> Bool
myOr' = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny fn = foldr
        (\a b ->
          if fn a == True
          then True
          else b) False

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' fn = foldr (\a b -> fn a) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr
           (\a b ->
           if a == x
           then True
           else b) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = any (==x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap fn = foldr 
           (\a b ->
             fn a : b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter fn = foldr 
              (\a b ->
                if fn a == True
                then a : b
                else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap fn = foldr (\a b -> (fn a) ++ b) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering)
            -> [a]
            -> a
myMaximumBy fn (x:xs) = foldr
                        (\a b ->
                          if fn a b == GT
                          then b
                          else a) x xs 
