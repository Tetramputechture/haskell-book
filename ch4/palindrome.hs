module Palindrome where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

