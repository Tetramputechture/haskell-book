module Instances where

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a))
                => f a 
                -> Bool
functorIdentity f =
  fmap id f == f

functorCompose :: (Functor f, Eq (f c)) 
               => f a
               -> Fun a b
               -> Fun b c
               -> Bool
functorCompose x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x) 

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type IdFC = 
        (Identity Int)
     -> IntToInt
     -> IntToInt
     -> Bool

checkIdentity :: IO ()
checkIdentity = do 
  quickCheck (functorIdentity :: (Identity Int) -> Bool)
  quickCheck (functorCompose :: IdFC)

data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

genPair :: Arbitrary a => Gen (Pair a)
genPair = do
  a <- arbitrary
  b <- arbitrary
  return $ Pair a b

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = genPair

type PairFC =
          (Pair Int)
       -> IntToInt
       -> IntToInt
       -> Bool

checkPair :: IO ()
checkPair = do
  quickCheck (functorIdentity :: (Pair Int) -> Bool)
  quickCheck (functorCompose :: PairFC)

data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
  a <- arbitrary
  b <- arbitrary
  return $ Two a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = genTwo

type TwoFC =
         (Two Int Int)
      -> IntToInt
      -> IntToInt
      -> Bool

checkTwo :: IO ()
checkTwo = do
  quickCheck (functorIdentity :: (Two Int Int) -> Bool)
  quickCheck (functorCompose :: TwoFC)


