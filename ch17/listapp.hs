module Listapp where

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil

instance Semigroup (List a) where
  Nil <> x          = x
  x <> Nil          = x
  (Cons x xs) <> ys = Cons x $ xs <> ys

instance Functor List where
  fmap _ Nil = Nil  
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs = (f <$> xs) <> (fs <*> xs)

