module Valid where

data Validation e a =
    Failure' e
  | Success' a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure' f) = Failure' f
  fmap f (Success' x) = Success' (f x)

instance Semigroup e
      => Applicative (Validation e) where
  pure = Success'
  (Success' f) <*> (Success' x) = Success' (f x)
  (Failure' x) <*> (Failure' y) = Failure' (x <> y)
  (Failure' x) <*> _ = Failure' x
  _ <*> (Failure' x) = Failure' x
