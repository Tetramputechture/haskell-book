module Exercises where

import Hangman
import Data.List
import Data.Char
import Test.QuickCheck


prop_dollar :: (Eq b) => (a -> b) -> a -> Bool
prop_dollar f x = (f $ x) == f x

prop_composition :: (Eq c) => (b -> c) -> (a -> b) -> a -> Bool
prop_composition f g x = ((f .g) x) == (f (g x))

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = toUpper x : xs

prop_capitalizeIdempotent :: String -> Bool
prop_capitalizeIdempotent xs =
  (capitalizeWord xs == twice capitalizeWord xs)
  &&
  (capitalizeWord xs == fourTimes capitalizeWord xs)

prop_sortIdempotent :: Ord a => [a] -> Bool
prop_sortIdempotent x = 
  (sort x == twice sort x)
  &&
  (sort x == fourTimes sort x)

data Fool =
    Fulse 
  | Frue
  deriving (Eq, Show)

genFool :: Gen Fool
genFool = oneof [return Fulse, return Frue]

genFool' :: Gen Fool
genFool' = frequency [(2, return Fulse), (1, return Frue)]

instance Arbitrary Fool where
  arbitrary = genFool'

testGen :: IO ()
testGen = do
  sample genFool
runSpecs :: IO ()
runSpecs = do
  quickCheck (prop_dollar (+1) :: Int -> Bool)
  quickCheck (prop_composition (+1) (+2) :: Int -> Bool)
  quickCheck prop_capitalizeIdempotent
  quickCheck (prop_sortIdempotent :: [Int] -> Bool)
  
