module Mc91 where

mc91 :: Integral t => t -> t
mc91 x 
  | x > 100 = x - 10
  | otherwise = mc91 $ mc91 $ x + 11
