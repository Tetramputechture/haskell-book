module Exercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- nope

data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

nopeInstance :: Nope (Int, Int, Int)
nopeInstance = undefined

-- PhEither 

data PhEither b a =
    PhLeft a
  | PhRight b
  deriving (Eq, Show)

instance Functor (PhEither a) where
  fmap _ (PhRight a) = PhRight a
  fmap f (PhLeft a) = PhLeft $ f a

instance Applicative (PhEither a) where
  pure = PhLeft
  PhRight a <*> _ = PhRight a
  _ <*> PhRight a = PhRight a
  PhLeft f <*> PhLeft a = PhLeft $ f a

instance Monad (PhEither a) where
  return = pure
  PhRight a >>= _ = PhRight a
  PhLeft a >>= k = k a

instance (Arbitrary a,
          Arbitrary b) 
      => Arbitrary (PhEither a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ PhRight a)
              ,(3, return $ PhLeft b)]

instance (Eq a, Eq b) 
      => EqProp (PhEither a b) where
  (=-=) = eq

phEitherInstance :: PhEither (Int, Int, Int) (Int, Int, Int)
phEitherInstance = undefined

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity $ f a

instance Monad Identity where
  return = pure
  Identity a >>= k = k a

instance Arbitrary a 
      => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

idInstance :: Identity (Int, Int, Int)
idInstance = undefined

-- list

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

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

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  lst >>= k = flatMap k lst

instance Arbitrary a 
      => Arbitrary (List a) where
  arbitrary =
    frequency [(1, pure Nil),
               (5, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where (=-=) = eq

listInstance :: List (Int, Int, Int)
listInstance = undefined

runTests :: IO ()
runTests = do
  -- nope tests
  quickBatch $ functor nopeInstance
  quickBatch $ applicative nopeInstance
  quickBatch $ monad nopeInstance  

  -- pEither tests
  quickBatch $ functor phEitherInstance
  quickBatch $ applicative phEitherInstance
  quickBatch $ monad phEitherInstance

  -- identity tests
  quickBatch $ functor idInstance
  quickBatch $ applicative idInstance
  quickBatch $ monad idInstance

  -- list tests
  quickBatch $ functor listInstance
  quickBatch $ applicative listInstance
  quickBatch $ monad listInstance

-- j 

j :: Monad m => m (m a) -> m a
j = (=<<) (id <$>) 

-- l1

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f xs = xs >>= (\x -> return (f x))
