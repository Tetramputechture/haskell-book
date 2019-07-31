module Apl1 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Monoid a
      => Monoid (ZipList a) where
  mempty = pure mempty

instance Semigroup a
      => Semigroup (ZipList a) where
  a <> b = (<>) <$> a <*> b

instance Eq a
      => EqProp (ZipList a) where
  (=-=) = eq

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
