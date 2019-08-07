module EitherMonad where

data Sum a b =
    First a 
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second a) = Second $ f a

instance Applicative (Sum a) where
  pure = Second
  First a <*> _ = First a
  _ <*> First a = First a
  Second f <*> Second a = Second $ f a

instance Monad (Sum a) where
  return = pure
  (Second a) >>= k = k a
  (First a) >>= _  = First a 

type Founded = Int

type Coders = Int

data SoftwareShop = 
  Shop {
      founded     :: Founded
    , programmers :: Coders
  } deriving (Eq, Show)

data FoundedError =
    NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded
  :: Int
  -> Sum FoundedError Founded
validateFounded n 
  | n < 0   = First $ NegativeYears n
  | n > 500 = First $ TooManyYears n
  | otherwise = Second n

validateCoders 
  :: Int
  -> Sum FoundedError Coders
validateCoders n
  | n < 0     = First $ NegativeCoders n
  | n > 5000  = First $ TooManyCoders n
  | otherwise = Second n

mkSoftware
  :: Int
  -> Int
  -> Sum FoundedError SoftwareShop
mkSoftware years coders = do
  founded     <- validateFounded years
  programmers <- validateCoders coders
  if programmers > div founded 10
    then First $
           TooManyCodersForYears
           founded programmers
    else Second $ Shop founded programmers

