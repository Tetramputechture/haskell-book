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

-- instance Applicative List where
--  pure a = Cons a Nil
--   Nil <*> _ = Nil
--  _ <*> Nil = Nil
--  (Cons f fs) <*> xs = (f <$> xs) <> (fs <*> xs)

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
  fs <*> xs = flatMap (<$> xs) fs
