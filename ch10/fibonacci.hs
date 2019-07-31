module Finonacci where

fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

fibsTwenty = take 20 fibs

fibsLessThan100 = takeWhile (\x -> x < 100) fibs

factorial = scanl (*) 1 [1..]
factorialN x = factorial !! x
