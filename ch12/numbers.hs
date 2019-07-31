module Numbers where

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x >= 0 = Just (integerToNat' x)
  | otherwise = Nothing

integerToNat' :: Integer -> Nat
integerToNat' 0 = Zero
integerToNat' x = Succ $ integerToNat' (x - 1)
