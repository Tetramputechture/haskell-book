module Equality where

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==)  (TisAn a)
        (TisAn b) =
      a == b

data TwoIntegers =
  Two Integer Integer
  
instance Eq TwoIntegers where
  (==)  (Two a b)
        (Two c d) =
    a == c && b == d

data StringOrInt =
    TisAnInt   Int
  | TisAString String

instance Eq StringOrInt where
  (==)  (TisAnInt a)
        (TisAnInt b) =
      a == b

  (==)  (TisAString str1) 
        (TisAString str2) =
      str1 == str2

  (==) _ _ = False

data Pair a =
  Pair a a 

instance Eq a => Eq (Pair a) where
  (==)  (Pair x y)
        (Pair z w) =
       x == z

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==)  (Tuple x y) 
        (Tuple z w) =
      x == z && y == w

data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==)  (ThisOne x)
        (ThisOne y) =
      x == y
  
  (==)  (ThatOne x)
        (ThatOne y) =
      x == y

  (==)  _ _ = False

data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==)  (Hello x)
        (Hello y) =
      x == y

  (==)  (Goodbye z) 
        (Goodbye w) =
      z == w

  (==) _ _ = False
