module Exercises where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a
      => Monoid (Optional a) where
  mempty = Nada

instance Semigroup a
      => Semigroup (Optional a) where
  Nada <> (Only a) = Only a
  (Only a) <> Nada = Only a
  (Only a) <> (Only a') = Only (a <> a')
  Nada <> Nada = Nada

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbin' e adv noun adj =
  e   <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."

madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinBetter' e adv noun adj =
  mconcat [e
          , "! he said "
          , adv
          , " as he jumped into his car "
          , noun
          , " and drove off with his "
          , adj
          , " wife."]

monoidAssoc :: (Eq m, Monoid m)
            => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m)
                   => m 
                   -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m)
                    => m
                    -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Bull =
    Fools 
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools

instance Semigroup Bull where
  _ <> _ = Fools

type BullMappend =
  Bull -> Bull -> Bull -> Bool

checkBull :: IO ()
checkBull = do
  let ma = monoidAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (ma :: BullMappend)
  quickCheck (mli :: Bull -> Bool)
  quickCheck (mlr :: Bull -> Bool)

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada

instance Semigroup (First' a) where
  (First' Nada) <> a = a
  a <> _ = a
    
firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary =
    frequency [ (1, return Nada)
              , (3, liftM Only arbitrary)]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = 
    frequency [ (1, return $ First' Nada)
              , (3, liftM First' arbitrary) ]

checkFirst :: IO ()
checkFirst = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

semigroupAssoc :: (Eq s, Semigroup s)
               => s -> s -> s -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

data Trivial = Trivial deriving (Eq, Show)

instance Monoid Trivial where
  mempty = Trivial

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

checkTrivial :: IO ()
checkTrivial = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

newtype Identity a = Identity a deriving (Eq, Show)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity $ mempty

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity a') = Identity (a <> a')

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = liftM Identity arbitrary

type IdentityAssoc =
  (Identity String) -> (Identity String) -> (Identity String) -> Bool

checkIdentity :: IO ()
checkIdentity = do
  let sa = semigroupAssoc
      mli = monoidRightIdentity
      mri = monoidRightIdentity
  quickCheck (sa :: IdentityAssoc)
  quickCheck (mli :: (Identity String) -> Bool)
  quickCheck (mri :: (Identity String) -> Bool)

data Two a b = Two a b deriving (Eq, Show)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

twoGen :: (Arbitrary a,
           Arbitrary b) =>
          Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

instance (Arbitrary a, 
          Arbitrary b) => 
         Arbitrary (Two a b) where
  arbitrary = twoGen 

type TwoAssoc = 
  (Two String String) -> (Two String String) -> (Two String String) -> Bool

checkTwo :: IO ()
checkTwo = do
  let sa = semigroupAssoc
      mli = monoidRightIdentity
      mri = monoidRightIdentity 
  quickCheck (sa :: TwoAssoc)
  quickCheck (mli :: (Two String String) -> Bool)
  quickCheck (mri :: (Two String String) -> Bool)

newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Monoid BoolConj where
  mempty = BoolConj True

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

genBoolConj :: Gen BoolConj
genBoolConj = oneof [return $ BoolConj True, 
                     return $ BoolConj False]

instance Arbitrary BoolConj where
  arbitrary = genBoolConj 

type BoolConjAssoc =
  BoolConj -> BoolConj -> BoolConj -> Bool

checkBoolConj :: IO ()
checkBoolConj = do
  let sa = semigroupAssoc
      mli = monoidRightIdentity
      mri = monoidRightIdentity
  quickCheck (sa :: BoolConjAssoc)
  quickCheck (mli :: BoolConj -> Bool)
  quickCheck (mri :: BoolConj -> Bool)

newtype BoolDisj =
  BoolDisj Bool 
  deriving (Eq, Show)

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

genBoolDisj :: Gen BoolDisj
genBoolDisj = oneof [return $ BoolDisj True, 
                     return $ BoolDisj False]

instance Arbitrary BoolDisj where
  arbitrary = genBoolDisj 

type BoolDisjAssoc =
  BoolDisj -> BoolDisj -> BoolDisj -> Bool

checkBoolDisj :: IO ()
checkBoolDisj = do
  let sa = semigroupAssoc
      mli = monoidRightIdentity
      mri = monoidRightIdentity
  quickCheck (sa :: BoolDisjAssoc)
  quickCheck (mli :: BoolDisj -> Bool)
  quickCheck (mri :: BoolDisj -> Bool)

data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Snd a) <> _ = Snd a
  _ <> a = a

genOr :: (Arbitrary a,
          Arbitrary b) =>
         Gen (Or a b)
genOr = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ Fst a,
         return $ Snd b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = genOr

type OrAssoc =
  (Or String String) -> (Or String String) -> (Or String String) -> Bool

checkOr :: IO ()
checkOr =
  quickCheck (semigroupAssoc :: OrAssoc)

newtype Combine a b =
  Combine { unCombine :: (a -> b) }
-- example usage
-- > let f = Combine $ \n -> Sum (n + 1)
-- > unCombine f $ 1 
-- 2

-- dummy show instance so quickCheck works
instance Show (Combine a b) where 
  show (Combine f) = "Combine instance"

-- monoid instance for Combine
instance Monoid b => Monoid (Combine a b) where
  mempty = Combine (const mempty)

-- semigroup definition for Combine
instance Semigroup b => Semigroup (Combine a b) where
  (Combine a) <> (Combine a') = Combine (\x -> a x <> a' x)

-- function asserting associative property for Combine
combineAssoc :: (Eq b, Semigroup b) 
             => a 
             -> Combine a b
             -> Combine a b
             -> Combine a b
             -> Bool
combineAssoc v f g h =
  (unCombine (f <> (g <> h)) $ v) == (unCombine ((f <> g) <> h) $ v)

-- Arbitrary instance for Combine
instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

-- Helper type for brevity when defining what we give to quickCheck
type CombineString = Combine String String

-- what we give to quickCheck when proving combine associativity
type CombineStringAssoc =
  String -> CombineString -> CombineString -> CombineString -> Bool

-- function asserting left identity for Combine
combineLeftIdentity :: (Eq b, Monoid b)
                    => a
                    -> Combine a b
                    -> Bool
combineLeftIdentity v f = (unCombine (mempty <> f) $ v) == (unCombine f $ v)

-- function asserting right identity for Combine
combineRightIdentity :: (Eq b, Monoid b)
                     => a
                     -> Combine a b
                     -> Bool
combineRightIdentity v f = (unCombine (f <> mempty) $ v) == (unCombine f $ v)

-- proving that Combine is a Monoid
checkCombine :: IO ()
checkCombine = do 
  quickCheck (combineAssoc :: CombineStringAssoc)
  quickCheck (combineLeftIdentity :: String -> CombineString -> Bool)
  quickCheck (combineRightIdentity :: String -> CombineString -> Bool)

newtype Comp a =
  Comp { unComp :: (a -> a) }

-- dummy show instance so quickCheck works
instance Show (Comp a) where 
  show (Comp a) = "Comp instance"

-- monoid instance for Comp
instance Monoid a => Monoid (Comp a) where
  mempty = Comp (const mempty)

-- semigroup definition for Comp
instance (Semigroup a) => Semigroup (Comp a) where
  (Comp a) <> (Comp a') = Comp (\x -> a x <> a' x)

-- function asserting associative property for Comp
compAssoc :: (Eq b, Semigroup b) 
          => b 
          -> Comp b
          -> Comp b
          -> Comp b
          -> Bool
compAssoc v f g h =
  (unComp (f <> (g <> h)) $ v) == (unComp ((f <> g) <> h) $ v)

-- Arbitrary instance for Comp
instance (Arbitrary a, CoArbitrary a) => Arbitrary (Comp a) where
  arbitrary = Comp <$> arbitrary

-- Helper type for brevity when defining what we give to quickCheck
type CompString = Comp String

-- what we give to quickCheck when proving comp associativity
type CompStringAssoc =
  String -> CompString -> CompString -> CompString -> Bool

-- function asserting left identity for Comp
compLeftIdentity :: (Eq a, Monoid a)
                 => a
                 -> Comp a
                 -> Bool
compLeftIdentity v f = (unComp (mempty <> f) $ v) == (unComp f $ v)

-- function asserting right identity for Comp
compRightIdentity :: (Eq a, Monoid a)
                  => a
                  -> Comp a
                  -> Bool
compRightIdentity v f = (unComp (f <> mempty) $ v) == (unComp f $ v)

-- proving that Comp is a Monoid
checkComp :: IO ()
checkComp = do 
  quickCheck (compAssoc :: CompStringAssoc)
  quickCheck (compLeftIdentity :: String -> CompString -> Bool)
  quickCheck (compRightIdentity :: String -> CompString -> Bool)

data Validation a b =
  Failure a | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Exercises.Failure a) <> (Exercises.Failure a') = Exercises.Failure (a <> a')
  s@(Exercises.Success b) <> _            = s
  _ <> s@(Exercises.Success b)            = s

genValidation :: (Arbitrary a,
                  Arbitrary b) =>
                 Gen (Validation a b)
genValidation = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ Exercises.Failure a,
         return $ Exercises.Success b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = genValidation

type ValidationAssoc =
  (Validation String Int) -> (Validation String Int) -> (Validation String Int) -> Bool

checkValidation :: IO ()
checkValidation = do
  quickCheck (semigroupAssoc :: ValidationAssoc)
  let failure :: String 
              -> Validation String Int
      failure = Exercises.Failure
      success :: Int
              -> Validation String Int
      success = Exercises.Success
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2

newtype Mem s a =
  Mem {
    runMem :: s -> (a, s)
  }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \x -> (mempty, x)

instance Semigroup a => Semigroup (Mem s a) where
  f <> g = Mem go
    where go x = ((fst $ runMem f x) <> (fst $ runMem g x),
                  snd $ runMem f $ (snd $ runMem g x))
 
checkMem :: IO ()
checkMem = do
  let f = Mem $ \x -> ("hi", x + 1)
      rmzero = runMem mempty 0
      rmleft = runMem (f <> mempty) 0
      rmright = runMem (mempty <> f) 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f 0
  print $ rmright == runMem f 0

