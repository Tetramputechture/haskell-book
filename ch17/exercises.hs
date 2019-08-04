module Exercises where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- | pair

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure a = Pair a a
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

instance Arbitrary a 
      => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

pairInstance = Pair ("a", "a", "a") ("a", "a", "a")

-- | two

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Monoid a, Semigroup a) 
      => Applicative (Two a) where
  pure a = Two mempty a
  (Two a f) <*> (Two x y) = Two (a <> x) (f y)

instance (Arbitrary a, Arbitrary b)
      => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Eq a, Eq b) 
      => EqProp (Two a b) where
  (=-=) = eq

twoInstance = Two ("a", "a", "a") ("b", "b", "b")

-- | three

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Semigroup a
        , Monoid b, Semigroup b)
      => Applicative (Three a b) where
  pure a = Three mempty mempty a
  (Three a b f) <*> (Three x y z) = 
    Three (a <> x) (b <> y) (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c)
      => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c)
      => EqProp (Three a b c) where
  (=-=) = eq

threeInstance = Three ("a", "a", "a")
                      ("b", "b", "b")
                      ("c", "c", "c")

-- | three'

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Monoid a, Semigroup a)
      => Applicative (Three' a) where
  pure a = Three' mempty a a 
  (Three' a f g) <*> (Three' x y z) =
    Three' (a <> x) (f y) (g z)

instance (Arbitrary a, Arbitrary b) 
      => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Three' a b b

instance (Eq a, Eq b) 
      => EqProp (Three' a b) where
  (=-=) = eq

threePrimeInstance = Three' ("a", "a", "a")
                            ("b", "b", "b")
                            ("b", "b", "b")

-- | four 

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four x y z w) = Four x y z (f w)

instance (Monoid a, Semigroup a
        , Monoid b, Semigroup b
        , Monoid c, Semigroup c) 
      => Applicative (Four a b c) where
  pure a = Four mempty mempty mempty a
  (Four a b c f) <*> (Four x y z w) =
    Four (a <> x) (b <> y) (c <> z) (f w)

instance (Arbitrary a
        , Arbitrary b
        , Arbitrary c
        , Arbitrary d)
      => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary 
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Eq a
        , Eq b
        , Eq c
        , Eq d)
      => EqProp (Four a b c d) where
  (=-=) = eq

fourInstance = Four ("a", "a", "a")
                    ("b", "b", "b")
                    ("c", "c", "c")
                    ("d", "d", "d")

-- | four'

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x y z w) = Four' x y z (f w)

instance (Monoid a, Semigroup a)
      => Applicative (Four' a) where
  pure a = Four' mempty mempty mempty a
  (Four' a b c f) <*> (Four' x y z w) =
    Four' (a <> x) (b <> y) (c <> z) (f w)

instance (Arbitrary a, Arbitrary b)
      => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Four' a a a b

instance (Eq a, Eq b) 
      => EqProp (Four' a b) where
  (=-=) = eq

fourPrimeInstance = Four' ("a", "a", "a")
                          ("a", "a", "a")
                          ("a", "a", "a")
                          ("b", "b", "b")

runTests :: IO ()
runTests = do
  putStr "Pair applicative"
  quickBatch $ applicative pairInstance
  
  putStr "Two applicative"
  quickBatch $ applicative twoInstance

  putStr "Three applicative"
  quickBatch $ applicative threeInstance

  putStr "Three' applicative"
  quickBatch $ applicative threePrimeInstance

  putStr "Four applicative"
  quickBatch $ applicative fourInstance

  putStr "Four' applicative"
  quickBatch $ applicative fourPrimeInstance
