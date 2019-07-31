module Lwc where

tensDigit :: Integral a => a -> a
tensDigit x = fst (x `divMod` 10) `mod` 10

hunsDigit :: Integral a => a -> a
hunsDigit x = fst (x `divMod` 100) `mod` 100

foldBool1 :: a -> a -> Bool -> a
foldBool1 x y z
  | z == False = x
  | otherwise = y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x _ False = x
foldBool2 _ y True = y

g :: (a -> b) -> (a, c) -> (b, c)
g fn (a, c) = (fn a, c)



