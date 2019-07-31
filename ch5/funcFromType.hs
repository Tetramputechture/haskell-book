module FuncFromType where

i :: a -> a
i x = x

c :: a -> b -> a
c x y = x

c' :: a -> b -> b
c' x y = y

r :: [a] -> [a]
r xs = reverse xs

r' :: [a] -> [a]
r' xs = tail xs

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC $ aToB a 

a :: (a -> c) -> a -> a
a aToC a = a

a' :: (a -> b) -> a -> b
a' aToB a = aToB a
