module Listapp where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

toMyList :: [a] -> List a
toMyList = foldr Cons Nil

instance Monoid (List a) where
  mempty = Nil

instance Semigroup (List a) where
  Nil <> x          = x
  x <> Nil          = x
  (Cons x xs) <> ys = Cons x $ xs <> ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
  Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b)
        -> List a
        -> List b
flatMap f = concat' . fmap f

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  --  (Cons f fs) <*> xs = (f <$> xs) <> (fs <*> xs)
  fs <*> xs = flatMap (<$> xs) fs

instance Arbitrary a 
      => Arbitrary (List a) where
  arbitrary =
    frequency [(1, pure Nil),
               (5, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where (=-=) = eq
xsTest = toMyList [("a", "b", "c")]

testListApplicative :: IO ()
testListApplicative = quickBatch $ applicative xsTest

-- | ziplist

take' :: Int -> List a -> List a
take' n Nil = Nil
take' 0 _ = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs) 

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' $ repeat' a
  (ZipList' Nil) <*> _ = ZipList' Nil
  _ <*> (ZipList' Nil) = ZipList' Nil
  -- fs <*> xs = fmap (\f -> take' 1 (f <$> pure xs)) fs
  (ZipList' fs) <*> (ZipList' ys) = ZipList' $ zipWith' ($) fs ys

repeat' :: a -> List a
repeat' a = Cons a (repeat' a)

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' _ Nil _ = Nil
zipWith' _ _ Nil = Nil
zipWith' fn (Cons x xs) (Cons y ys) = Cons (fn x y) (zipWith' fn xs ys)

instance Arbitrary a 
      => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

testZipListApplicative :: IO ()
testZipListApplicative = quickBatch $ applicative (ZipList' xsTest)
