module Recursion where

sumUpTo :: Integral a => a -> a
sumUpTo n 
  | n <= 1 = 1
  | otherwise = n + (sumUpTo (n - 1))

mult :: Integral a => a -> a -> a
mult x y
  | y == 0 = 0
  | y == 1 = x
  | otherwise = x + (mult x (y - 1))

